module Fpex.Main where

import Control.Monad.Extra
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Fpex.Collect as Collect
import qualified Fpex.Course.CourseSetup as Setup
import Fpex.Course.Types
import qualified Fpex.Eval.Types as Eval
import qualified Fpex.Grade as Grade
import Fpex.Options
import qualified Fpex.Publish as Publish
import Fpex.Reporter
import qualified Fpex.Stats.Csv as Stats
import qualified Fpex.Stats.Grade as Stats
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO (stderr)

defaultMain :: IO ()
defaultMain = (runM . runError $ defaultMain') >>= \case
  Right () -> return ()
  Left e -> do
    T.hPutStrLn stderr e
    exitFailure

defaultMain' :: Members '[Error Text, Embed IO] r => Sem r ()
defaultMain' = do
  opts <- embed $ execParser options
  case optionCommand opts of
    Setup setupCommand -> Setup.courseSetup setupCommand
    FinalPoints FinalPointsCommand {..} -> do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      embed $ setCurrentDirectory courseDir
    Lc gradeTestSuiteOptions lifecycle -> do
      (Course {..}, courseDir) <- getCourseConfig (optionCourseFile opts)
      let students = maybe courseParticipants pure (optionStudent opts)
      embed $ setCurrentDirectory courseDir
      dispatchLifeCycle Course {..} students gradeTestSuiteOptions lifecycle

dispatchLifeCycle ::
  Members '[Error Text, Embed IO] r =>
  Course ->
  [Student] ->
  TestSuiteOptions ->
  LifeCycle ->
  Sem r ()
dispatchLifeCycle course students TestSuiteOptions {..} lifecycle = do
  let testSuiteName = optionTestSuiteName
  case lifecycle of
    Grade GradeCommand {..} -> runReader course $ do
      whenJust gradeTestSuite $ setTestSuite optionSubmissionId testSuiteName
      errorReports <- withReport "run errorStudent" $ do
        Grade.runGradeError
          ( Grade.createEmptyStudent
              optionSubmissionId
              testSuiteName
          )
          >>= \case
            Right errorReport -> return errorReport
            Left err -> throw $
              case err of
                Grade.RunnerError msg serr ->
                  T.unlines $
                    [ "Failed to execute neutral student",
                      msg,
                      "Stderr: ",
                      T.pack $ LBS.unpack serr
                    ]
                Grade.FailedToDecodeJsonResult msg ->
                  T.unlines
                    ["Failed to decode the json result: ", T.pack msg]
                Grade.NoSubmission ->
                  "Main.hs:Grade (NoSubmission) Invariant violated, can not be generated here."
      forM_ students $ \student -> do
        withReport ("run testsuite for student " <> studentId student) $ do
          let subInfo =
                Eval.SubmissionInfo
                  { Eval.subStudent = student,
                    Eval.subId = optionSubmissionId,
                    Eval.subTestSuite = testSuiteName
                  }
          runReader errorReports $ runReader subInfo $ do
            submissionResult <- Grade.runSubmission
            Grade.prettyTestReport submissionResult
    Collect CollectCommand -> do
      embed $
        Collect.prepareSubmissionFolder
          optionSubmissionId
          testSuiteName
      _ <-
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
      embed
        $ putStrLn
        $ "Collected "
          <> show (length collected)
          <> "/"
          <> show (length students)
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
              optionSubmissionId
              course
              testSuiteName
          )
      case statOutputKind of
        StatsOutputCsv -> embed (T.putStrLn $ Stats.statsCsv stats)
        StatsOutputGrades ->
          embed (T.putStrLn $ Stats.statsGrade stats)
    RecalculatePoints ->
      forM_ students $ \student -> do
        withReport ("Calculate points for: " <> studentId student) $ do
          let sinfo =
                Eval.SubmissionInfo
                  { Eval.subStudent = student,
                    Eval.subId = optionSubmissionId,
                    Eval.subTestSuite = testSuiteName
                  }
          runReader sinfo $ do
            mts <- Eval.readTestSuiteResult
            case mts of
              Nothing -> throw "Inconsistent state, could not decode test-suite."
              Just ts -> do
                let newTs = Eval.recalculateTestPoints ts
                Eval.writeTestSuiteResult newTs
    DiffResults DiffResultsCommand {..} -> do
      let oldSid = diffResultSid
      let currentSid = optionSubmissionId
      let suiteName = optionTestSuiteName
      embed $ T.putStrLn $
        "Show Difference between " <> T.pack (show $ Eval.getSubmissionId currentSid)
          <> " and "
          <> T.pack (show $ Eval.getSubmissionId oldSid)
      forM_ students $ \student -> embed $ do
        moldReport <- Aeson.decodeFileStrict $ Eval.reportSourceJsonFile oldSid suiteName student
        mnewReport <- Aeson.decodeFileStrict $ Eval.reportSourceJsonFile currentSid suiteName student
        let oldReport = fromMaybe (error "TODO") moldReport
        let newReport = fromMaybe (error "TODO") mnewReport
        let oldSubmission = Eval.assignmentCollectStudentFile oldSid suiteName student
        let newSubmission = Eval.assignmentCollectStudentFile currentSid suiteName student
        let correctTests = Eval.correctTests newReport - Eval.correctTests oldReport
        let failedTests = Eval.failedTests newReport - Eval.failedTests oldReport
        let notSubmittedTests = Eval.notSubmittedTests newReport - Eval.notSubmittedTests oldReport
        let timeoutTests = Eval.timeoutTests newReport - Eval.timeoutTests oldReport
        let oldScore = Eval.testSuitePoints oldReport
        let newScore = Eval.testSuitePoints newReport
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

-------------------------------------------------------------------------------------

setTestSuite :: Members [Error Text, Embed IO] r => Eval.SubmissionId -> Text -> FilePath -> Sem r ()
setTestSuite optionSubmissionId testSuiteName testSuiteSpec = do
  testSuiteSpecification <- embed $ makeAbsolute testSuiteSpec
  checkTestSuiteExists testSuiteSpecification
  embed $ Collect.setTestSuite optionSubmissionId testSuiteName testSuiteSpecification

-------------------------------------------------------------------------------------

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
  (Member (Error Text) r, Member (Embed IO) r) =>
  Maybe FilePath ->
  Sem r (Course, FilePath)
getCourseConfig courseOption = do
  courseFile <- getCourseFile courseOption >>= \case
    Just c -> return c
    Nothing -> throw "course.json not found"
  embed (Aeson.decodeFileStrict courseFile) >>= \case
    Just c -> do
      ghciFile <- embed $ canonicalizePath $ courseGhciEnvironment c
      return (c {courseGhciEnvironment = ghciFile}, takeDirectory courseFile)
    Nothing -> throw "course.json invalid format"

checkTestSuiteExists ::
  Members '[Error Text, Embed IO] r => FilePath -> Sem r ()
checkTestSuiteExists testSuite =
  unlessM (embed $ doesFileExist testSuite)
    $ throw
    $ "Test-Suite specification \""
      <> T.pack testSuite
      <> "\" does not exist"
