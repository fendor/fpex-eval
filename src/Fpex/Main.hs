module Fpex.Main where

import Colog.Actions
import Colog.Polysemy
import qualified Colog.Polysemy as Log
import Colourista
import Control.Monad.Extra
import qualified Data.Aeson as Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Fpex.Collect as Collect
import qualified Fpex.Course.CourseSetup as Setup
import qualified Fpex.Course.Student as Student
import Fpex.Course.Types
import qualified Fpex.Feedback as Feedback
import qualified Fpex.Grade as Grade
import Fpex.Grade.Analysis
import qualified Fpex.Grade.ErrorStudent as ErrorStudent
import Fpex.Grade.Paths as Paths
import qualified Fpex.Grade.Result as Grade
import qualified Fpex.Grade.Storage as Storage
import qualified Fpex.Grade.Tasty as Grade
import qualified Fpex.Grade.Types as Grade
import Fpex.Options
import Fpex.Publish.Stats
import Fpex.Reporter
import qualified Fpex.Stats.Csv as Stats
import qualified Fpex.Stats.Grade as Stats
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import System.Directory
import System.Exit (exitFailure)
import System.FilePath

defaultMain :: IO ()
defaultMain =
  (runM . runError . runLogActionSem logTextStderr $ defaultMain') >>= \case
    Right () -> return ()
    Left e -> do
      errorMessage e
      exitFailure

defaultMain' :: Members [Log.Log T.Text, Error Text, Embed IO] r => Sem r ()
defaultMain' = do
  opts <- embed $ execParser options
  case optionCommand opts of
    Setup setupCommand -> Setup.courseSetup setupCommand
    FinalPoints FinalPointsCommand {..} -> Storage.runStorageFileSystem $ do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      embed $ setCurrentDirectory courseDir

      outputDir <- embed $ canonicalizePath finalPointsOutput
      embed $ createDirectoryIfMissing True outputDir
      forM_ students $ \student -> runReader Course {..} $
        runReader student $
          Student.runFileSystemStudentDirectory $ do
            report <- studentPointReport finalPointsSubmissions finalPointsSubmissionIds student
            let prettyReport = renderPoints finalPointsSubmissions finalPointsSubmissionIds Mean student report
            embed $ T.writeFile (outputDir </> T.unpack (studentId student) <.> "md") prettyReport
            when (PublishFeedback <= finalPointsFeedback) $ do
              Student.publishFile prettyReport "finalPoints.md"
    Lc gradeTestSuiteOptions lifecycle -> do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      embed $ setCurrentDirectory courseDir

      runReader Course {..} $ dispatchLifeCycle students gradeTestSuiteOptions lifecycle

buildApp :: Members [Embed IO, Error T.Text] r => Course -> TestSuiteOptions -> Maybe StudentSubmission -> Sem r AppCtx
buildApp course TestSuiteOptions {..} studentSubmission = do
  let submission = buildStudentSubmissionWithDefault optionSubmissionName studentSubmission
  pure
    AppCtx
      { appCourse = course,
        appSubmissionId = optionSubmissionId,
        appSubmissionName = optionSubmissionName,
        appStudentSubmission = submission
      }

dispatchLifeCycle ::
  Members [Log.Log T.Text, Error Text, Embed IO, Reader Course] r =>
  [Student] ->
  TestSuiteOptions ->
  LifeCycle ->
  Sem r ()
dispatchLifeCycle students TestSuiteOptions {..} lifecycle = do
  let submissionName = optionSubmissionName
  let studentSubmission = buildStudentSubmissionWithDefault submissionName optionStudentSubmission
  runReader studentSubmission $
    case lifecycle of
      Grade GradeCommand {..} ->
        Storage.runStorageFileSystem $
          evalState (mempty :: AnalysisState) $
            runStatefulAnalyser $
              runReader (defaultRunnerInfo studentSubmission testTimeout) $
                do
                  -- Run Error Student, prepare failed submissions, etc...
                  let errorStudent = ErrorStudent.errorStudentSubmissionInfo optionSubmissionId submissionName
                  whenJust gradeTestSuite $ setTestSuite optionSubmissionId submissionName
                  errorReports <- withReport "run errorStudent" $ do
                    Grade.runGradeError
                      ( Grade.runTastyTestSuite $
                          Grade.createEmptyStudent gradeBaseDefinitions errorStudent
                      )
                      >>= \case
                        Right errorReport -> return errorReport
                        Left err -> throw =<< ErrorStudent.prettyRunnerError errorStudent err

                  -- Run testsuite grading for all students.
                  forM_ students $ \student -> do
                    withReport ("run testsuite for student " <> studentId student) $ do
                      let subInfo =
                            SubmissionInfo
                              { subStudent = student,
                                subId = optionSubmissionId,
                                subName = submissionName
                              }
                      runReader subInfo $
                        runReader errorReports $
                          Grade.runGradeError $
                            Grade.runTastyTestSuite $ do
                              submissionResult <- Grade.runSubmission
                              _warnings <- analyseTestSuite subInfo submissionResult
                              prettyTestReport submissionResult

                  analysisReport <- finalAnalysisReport
                  runReader errorReports $ printFinalAnalysisReport analysisReport
      Collect CollectCommand -> do
        embed $
          Collect.prepareSubmissionFolder
            optionSubmissionId
            submissionName

        embed $
          Collect.createEmptyStudent
            optionSubmissionId
            submissionName

        collectResults <- forM students $ \student -> do
          let sinfo = SubmissionInfo student optionSubmissionId optionSubmissionName
          Storage.runStorageFileSystem $
            runReader student $
              Student.runFileSystemStudentDirectory $
                Student.collectStudentSubmission
                  sinfo
                  studentSubmission

        let collected = filter isNothing collectResults
        embed $
          successMessage $
            "Collected "
              <> T.pack (show (length collected))
              <> "/"
              <> T.pack (show (length students))
              <> " submissions."
      Feedback FeedbackCommand {..} -> do
        Storage.runStorageFileSystem $
          forM_ students $ \student -> do
            let sinfo = SubmissionInfo student optionSubmissionId submissionName
            runReader sinfo $
              runReader student $
                Student.runFileSystemStudentDirectory $
                  Feedback.runFeedbackService $ do
                    Feedback.generateTestFeedback
                    when (PublishFeedback <= feedbackPublish) $ do
                      Student.publishTestSuiteResult sinfo studentSubmission
                      testSuiteLocation <- reportTestSuiteFile
                      Student.publishTestSuite sinfo studentSubmission testSuiteLocation
                      Student.publishSubmissionFeedback sinfo studentSubmission
      Stats StatCommand {..} -> do
        stats <-
          Storage.runStorageFileSystem
            ( Stats.collectData
                students
                optionSubmissionId
                submissionName
            )
        case statOutputKind of
          StatsOutputCsv -> embed (T.putStrLn $ Stats.statsCsv stats)
          StatsOutputGrades ->
            embed (T.putStrLn $ Stats.statsGrade stats)
      RecalculatePoints -> Storage.runStorageFileSystem $
        forM_ students $ \student -> do
          withReport ("Calculate points for: " <> studentId student) $ do
            let sinfo =
                  SubmissionInfo
                    { subStudent = student,
                      subId = optionSubmissionId,
                      subName = submissionName
                    }

            ts <- Storage.readTestSuiteResult sinfo
            let newTs = Grade.recalculateTestPoints ts
            Storage.writeTestSuiteResult sinfo newTs
      DiffResults DiffResultsCommand {..} -> Storage.runStorageFileSystem $ do
        let oldSid = diffResultSid
        let currentSid = optionSubmissionId
        embed $
          T.putStrLn $
            "Show Difference between " <> T.pack (show $ getSubmissionId currentSid)
              <> " and "
              <> T.pack (show $ getSubmissionId oldSid)
        forM_ students $ \student -> do
          oldReport <- Storage.readTestSuiteResult $ SubmissionInfo student oldSid submissionName
          newReport <- Storage.readTestSuiteResult $ SubmissionInfo student currentSid submissionName
          let oldSubmission = Paths.assignmentCollectStudentFile oldSid submissionName studentSubmission student
          let newSubmission = Paths.assignmentCollectStudentFile currentSid submissionName studentSubmission student
          let correctTests = Grade.correctTests newReport - Grade.correctTests oldReport
          let failedTests = Grade.failedTests newReport - Grade.failedTests oldReport
          let notSubmittedTests = Grade.notSubmittedTests newReport - Grade.notSubmittedTests oldReport
          let timeoutTests = Grade.timeoutTests newReport - Grade.timeoutTests oldReport
          let oldScore = Grade.testSuitePoints oldReport
          let newScore = Grade.testSuitePoints newReport
          if oldScore /= newScore || any (/= 0) [correctTests, failedTests, notSubmittedTests, timeoutTests]
            then embed $ do
              T.putStrLn $ "Difference for " <> studentId student
              T.putStrLn $ "  Old: " <> T.pack oldSubmission <> " -- Points: " <> T.pack (show oldScore)
              T.putStrLn $ "  New: " <> T.pack newSubmission <> " -- Points: " <> T.pack (show newScore)
              T.putStrLn $ "  ---"
              T.putStrLn $
                "  "
                  <> T.concat
                    [ "Correct: ",
                      T.pack $ show correctTests,
                      ", Incorrect: ",
                      T.pack $ show failedTests,
                      ", Not submitted: ",
                      T.pack $ show notSubmittedTests,
                      ", Timeout: ",
                      T.pack $ show timeoutTests
                    ]
            else embed $ T.putStrLn $ "No Difference (" <> T.pack (show newScore) <> " Points)"
          return ()
        return ()

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

setTestSuite ::
  Members [Error Text, Embed IO] r =>
  SubmissionId ->
  SubmissionName ->
  TestSuitePath ->
  Sem r ()
setTestSuite submissionId submissionName testSuiteSpec = do
  absoluteTestSuiteSpec <- checkTestSuiteExists testSuiteSpec
  embed $ Collect.setTestSuite submissionId submissionName absoluteTestSuiteSpec

defaultRunnerInfo :: StudentSubmission -> Timeout -> Grade.RunnerInfo
defaultRunnerInfo studentSubmission t =
  Grade.RunnerInfo
    { Grade.runnerInfoTimeout = t,
      Grade.runnerInfoReportOutput = "testsuite-result.json",
      Grade.runnerInfoStudentSubmission = studentSubmission
    }

buildStudentSubmissionWithDefault :: SubmissionName -> Maybe StudentSubmission -> StudentSubmission
buildStudentSubmissionWithDefault submissionName = \case
  Nothing ->
    StudentSubmission $ T.unpack (getSubmissionName submissionName) <.> "hs"
  Just s -> s

checkTestSuiteExists ::
  Members [Error Text, Embed IO] r => TestSuitePath -> Sem r TestSuitePath
checkTestSuiteExists testSuite = do
  let fp = getTestSuitePath testSuite
  afp <- embed $ canonicalizePath fp
  unlessM (embed $ doesFileExist afp) $
    throw $
      "Test-Suite specification \""
        <> T.pack afp
        <> "\" does not exist"
  pure $ TestSuitePath afp

-- ----------------------------------------------------------------------------
-- Project setup relevant functions
-- ----------------------------------------------------------------------------

-- | get the default course file
getDefaultCourseFile :: IO (Maybe FilePath)
getDefaultCourseFile = do
  cwd <- getCurrentDirectory
  let possibleCourseJsonFiles = map (</> "course.json") (Setup.ancestors cwd)
  findM doesFileExist possibleCourseJsonFiles

getCourseFile ::
  Members [Error Text, Embed IO] r =>
  Maybe FilePath ->
  Sem r (Maybe FilePath)
getCourseFile (Just c) = return $ Just c
getCourseFile Nothing = embed getDefaultCourseFile

getCourseConfig ::
  (Members [Error Text, Embed IO] r) =>
  Maybe FilePath ->
  Sem r (Course, FilePath)
getCourseConfig courseOption = do
  courseFile <-
    getCourseFile courseOption >>= \case
      Just c -> return c
      Nothing -> throw "course.json not found. Create one with 'fpex init'"
  embed (Aeson.decodeFileStrict courseFile) >>= \case
    Just c -> do
      ghciFile <- embed $ canonicalizePath $ courseGhciEnvironment c
      return (c {courseGhciEnvironment = ghciFile}, takeDirectory courseFile)
    Nothing -> throw "course.json invalid format"

-- ----------------------------------------------------------------------------
-- Project setup relevant functions
-- ----------------------------------------------------------------------------