module Fpex.Main where

import Colourista
import Control.Monad.Extra
import qualified Data.Aeson as Aeson
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Fpex.Collect as Collect
import qualified Fpex.Course.CourseSetup as Setup
import Fpex.Course.Types
import qualified Fpex.Grade as Grade
import Fpex.Grade.Analysis
import qualified Fpex.Grade.ErrorStudent as ErrorStudent
import qualified Fpex.Grade.Storage as Grade
import qualified Fpex.Grade.Types as Grade
import Fpex.Options
import qualified Fpex.Publish as Publish
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
  (runM . runError $ defaultMain') >>= \case
    Right () -> return ()
    Left e -> do
      errorMessage e
      exitFailure

defaultMain' :: Members [Error Text, Embed IO] r => Sem r ()
defaultMain' = do
  opts <- embed $ execParser options
  case optionCommand opts of
    Setup setupCommand -> Setup.courseSetup setupCommand
    FinalPoints FinalPointsCommand {..} -> do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      outputDir <- embed $ canonicalizePath finalPointsOutput
      embed $ setCurrentDirectory courseDir
      embed $ createDirectoryIfMissing True outputDir
      forM_ students $ \student -> do
        report <- embed $ studentPointReport finalPointsSubmissions finalPointsSubmissionIds student
        let prettyReport = renderPoints finalPointsSubmissions finalPointsSubmissionIds Mean student report
        embed $ T.writeFile (outputDir </> T.unpack (studentId student) <.> "md") prettyReport
    Lc gradeTestSuiteOptions lifecycle -> do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      embed $ setCurrentDirectory courseDir
      dispatchLifeCycle Course {..} students gradeTestSuiteOptions lifecycle

dispatchLifeCycle ::
  Members [Error Text, Embed IO] r =>
  Course ->
  [Student] ->
  TestSuiteOptions ->
  LifeCycle ->
  Sem r ()
dispatchLifeCycle course students TestSuiteOptions {..} lifecycle = do
  let testSuiteName = optionTestSuiteName
  case lifecycle of
    Grade GradeCommand {..} ->
      Grade.runTestSuiteStorageFileSystem $
        evalState (mempty :: AnalysisState) $
          runStatefulAnalyser $
            runReader (defaultRunnerInfo testTimeout) $
              runReader course $ do
                -- Run Error Student, prepare failed submissions, etc...
                let errorStudent = ErrorStudent.errorStudentSubmissionInfo optionSubmissionId testSuiteName
                whenJust gradeTestSuite $ setTestSuite optionSubmissionId testSuiteName
                errorReports <- withReport "run errorStudent" $ do
                  Grade.runGradeError
                    ( Grade.runTastyTestSuite $
                        Grade.createEmptyStudent gradeBaseDefinitions errorStudent
                    )
                    >>= \case
                      Right errorReport -> return errorReport
                      Left err -> throw =<< Grade.prettyRunnerError errorStudent err

                -- Run testsuite grading for all students.
                forM_ students $ \student -> do
                  withReport ("run testsuite for student " <> studentId student) $ do
                    let subInfo =
                          SubmissionInfo
                            { subStudent = student,
                              subId = optionSubmissionId,
                              subTestSuite = testSuiteName
                            }
                    runReader subInfo $
                      runReader errorReports $
                        Grade.runGradeError $
                          Grade.runTastyTestSuite $ do
                            submissionResult <- Grade.runSubmission
                            analyseTestSuite subInfo submissionResult
                            prettyTestReport submissionResult

                analysisReport <- finalAnalysisReport
                runReader errorReports $ printFinalAnalysisReport analysisReport
    Collect CollectCommand -> do
      embed $
        Collect.prepareSubmissionFolder
          optionSubmissionId
          testSuiteName

      embed $
        Collect.createEmptyStudent
          optionSubmissionId
          testSuiteName
      collectResults <- forM students $ \student -> do
        embed $
          Collect.collectSubmission
            optionSubmissionId
            course
            testSuiteName
            student
      let (_errs, collected) = partitionEithers collectResults
      embed $
        successMessage $
          "Collected "
            <> T.pack (show (length collected))
            <> "/"
            <> T.pack (show (length students))
            <> " submissions."
    Publish PublishCommand -> do
      forM_ students $ \student -> do
        embed $
          Publish.publishTestResult
            optionSubmissionId
            course
            testSuiteName
            student
    Stats StatCommand {..} -> do
      stats <-
        embed
          ( Stats.collectData
              students
              optionSubmissionId
              testSuiteName
          )
      case statOutputKind of
        StatsOutputCsv -> embed (T.putStrLn $ Stats.statsCsv stats)
        StatsOutputGrades ->
          embed (T.putStrLn $ Stats.statsGrade stats)
    RecalculatePoints -> Grade.runTestSuiteStorageFileSystem $
      forM_ students $ \student -> do
        withReport ("Calculate points for: " <> studentId student) $ do
          let sinfo =
                SubmissionInfo
                  { subStudent = student,
                    subId = optionSubmissionId,
                    subTestSuite = testSuiteName
                  }

          mts <- Grade.readTestSuiteResult sinfo
          case mts of
            Nothing -> throw "Inconsistent state, could not decode test-suite."
            Just ts -> do
              let newTs = Grade.recalculateTestPoints ts
              Grade.writeTestSuiteResult sinfo newTs
    DiffResults DiffResultsCommand {..} -> do
      let oldSid = diffResultSid
      let currentSid = optionSubmissionId
      let suiteName = optionTestSuiteName
      embed $
        T.putStrLn $
          "Show Difference between " <> T.pack (show $ getSubmissionId currentSid)
            <> " and "
            <> T.pack (show $ getSubmissionId oldSid)
      forM_ students $ \student -> embed $ do
        moldReport <-
          Aeson.decodeFileStrict $
            Grade.reportSourceJsonFile oldSid suiteName student
        mnewReport <-
          Aeson.decodeFileStrict $
            Grade.reportSourceJsonFile currentSid suiteName student
        let oldReport = fromMaybe (error "TODO") moldReport
        let newReport = fromMaybe (error "TODO") mnewReport
        let oldSubmission = Grade.assignmentCollectStudentFile oldSid suiteName student
        let newSubmission = Grade.assignmentCollectStudentFile currentSid suiteName student
        let correctTests = Grade.correctTests newReport - Grade.correctTests oldReport
        let failedTests = Grade.failedTests newReport - Grade.failedTests oldReport
        let notSubmittedTests = Grade.notSubmittedTests newReport - Grade.notSubmittedTests oldReport
        let timeoutTests = Grade.timeoutTests newReport - Grade.timeoutTests oldReport
        let oldScore = Grade.testSuitePoints oldReport
        let newScore = Grade.testSuitePoints newReport
        if oldScore /= newScore || any (/= 0) [correctTests, failedTests, notSubmittedTests, timeoutTests]
          then do
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
          else T.putStrLn $ "No Difference (" <> T.pack (show newScore) <> " Points)"
        return ()
      return ()

-- ----------------------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------------------

setTestSuite ::
  Members [Error Text, Embed IO] r =>
  SubmissionId ->
  Assignment ->
  FilePath ->
  Sem r ()
setTestSuite optionSubmissionId testSuiteName testSuiteSpec = do
  testSuiteSpecification <- embed $ makeAbsolute testSuiteSpec
  checkTestSuiteExists testSuiteSpecification
  embed $ Collect.setTestSuite optionSubmissionId testSuiteName testSuiteSpecification

defaultRunnerInfo :: Grade.Timeout -> Grade.RunnerInfo
defaultRunnerInfo t =
  Grade.RunnerInfo
    { Grade.runnerInfoTimeout = t,
      Grade.runnerInfoReportOutput = "testsuite-result.json"
    }

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

checkTestSuiteExists ::
  Members [Error Text, Embed IO] r => FilePath -> Sem r ()
checkTestSuiteExists testSuite =
  unlessM (embed $ doesFileExist testSuite) $
    throw $
      "Test-Suite specification \""
        <> T.pack testSuite
        <> "\" does not exist"
