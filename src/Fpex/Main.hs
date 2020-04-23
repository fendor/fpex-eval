module Fpex.Main where

import Control.Monad
  ( forM,
    forM_,
  )
import Control.Monad.Extra
  ( findM,
    unlessM,
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Either
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
import qualified Fpex.Stats.Csv as Stats
import qualified Fpex.Stats.Grade as Stats
import Options.Applicative
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO (hPutStrLn, stderr)

defaultMain :: IO ()
defaultMain = (runM . runError $ defaultMain') >>= \case
  Right () -> return ()
  Left e -> do
    T.hPutStrLn stderr e
    exitFailure

defaultMain' :: Members '[Error Text, Embed IO] r => Sem r ()
defaultMain' = do
  Options {..} <- embed $ execParser options
  case optionCommand of
    Setup setupCommand -> Setup.courseSetup setupCommand
    Lc gradeTestSuiteOptions lifecycle ->
      dispatchLifeCycle Options {..} gradeTestSuiteOptions lifecycle

dispatchLifeCycle ::
  Members '[Error Text, Embed IO] r =>
  Options ->
  TestSuiteOptions ->
  LifeCycle ->
  Sem r ()
dispatchLifeCycle Options {..} TestSuiteOptions {..} lifecycle = do
  let testSuiteName = optionTestSuiteName
  (Course {..}, courseDir) <- getCourseConfig optionCourseFile
  let students = maybe courseParticipants pure optionStudent
  embed $ setCurrentDirectory courseDir
  case lifecycle of
    Grade GradeCommand {..} -> runReader Course {..} $ do
      (compileFailTestSuite, noSubmissionTestSuite) <-
        Grade.runGradeError
          ( Grade.createEmptyStudent
              optionSubmissionId
              testSuiteName
          )
          >>= \case
            Right (a, b) -> return (a, b)
            Left err -> do
              case err of
                Grade.RunnerError msg serr -> embed $ do
                  T.putStrLn "Failed to execute neutral student"
                  T.putStrLn msg
                  LBS.putStrLn $ "Stderr: " <> serr
                Grade.FailedToDecodeJsonResult msg ->
                  embed $ hPutStrLn stderr msg
                Grade.NoSubmission ->
                  error "Main.hs:Grade (NoSubmission) Invariant violated, can not be generated here."
              throw $ T.pack $ show err
      forM_ students $ \student -> do
        embed $ T.putStrLn $ "run testsuite for student " <> studentId student
        let targetFile =
              Eval.reportSourceJsonFile
                optionSubmissionId
                testSuiteName
                student
        Grade.runGradeError
          ( Grade.runSubmission
              optionSubmissionId
              testSuiteName
              student
          )
          >>= \case
            Right testResult -> do
              embed $ T.putStrLn "\tTest Report:"
              embed $ T.putStrLn $
                "\t\tPoints: "
                  <> T.pack (show $ Eval.testSuitePoints testResult)
                  <> "/"
                  <> T.pack (show $ Eval.maxScore testResult)
              embed $ T.putStrLn $
                "\t\t"
                  <> T.concat
                    [ "Correct: ",
                      T.pack . show $ Eval.correctTests testResult,
                      ", Incorrect: ",
                      T.pack . show $ Eval.failedTests testResult,
                      ", Not submitted: ",
                      T.pack . show $ Eval.notSubmittedTests testResult,
                      ", Timeout: ",
                      T.pack . show $ Eval.timeoutTests testResult
                    ]
            Left err -> embed $ do
              T.putStr "\t"
              case err of
                Grade.RunnerError _ serr -> do
                  T.putStrLn "Runner Error"
                  LBS.putStrLn serr
                  Aeson.encodeFile
                    targetFile
                    compileFailTestSuite
                Grade.FailedToDecodeJsonResult msg ->
                  T.putStrLn $ T.pack msg
                Grade.NoSubmission -> do
                  T.putStrLn "No Submission"
                  Aeson.encodeFile
                    targetFile
                    noSubmissionTestSuite
        return ()
    Collect CollectCommand -> do
      embed $
        Collect.prepareSubmissionFolder
          optionSubmissionId
          testSuiteName

      collectResults <- forM students $ \student -> do
        embed $
          Collect.collectSubmission
            optionSubmissionId
            Course {..}
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
            Course {..}
            testSuiteName
            student
        return ()
    Stats StatCommand {..} -> do
      stats <-
        embed
          ( Stats.collectData
              optionSubmissionId
              Course {..}
              testSuiteName
          )
      case statOutputKind of
        StatsOutputCsv -> embed (T.putStrLn $ Stats.statsCsv stats)
        StatsOutputGrades ->
          embed (T.putStrLn $ Stats.statsGrade stats)
    SetTestSuite SetTestSuiteCommand {..} -> do
      testSuiteSpecification <- embed $ makeAbsolute setTestSuiteSpecification
      checkTestSuiteExists testSuiteSpecification
      embed $ Collect.setTestSuite optionSubmissionId testSuiteName testSuiteSpecification

-- | get the default course file
getDefaultCourseFile :: IO (Maybe FilePath)
getDefaultCourseFile = do
  cwd <- getCurrentDirectory
  let possibleCourseJsonFiles = map (</> "course.json") (Setup.ancestors cwd)
  findM doesFileExist possibleCourseJsonFiles

getCourseFile ::
  (Member (Error Text) r, Member (Embed IO) r) =>
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
    Just c -> return (c, takeDirectory courseFile)
    Nothing -> throw "course.json invalid format"

checkTestSuiteExists ::
  Members '[Error Text, Embed IO] r => FilePath -> Sem r ()
checkTestSuiteExists testSuite =
  unlessM (embed $ doesFileExist testSuite)
    $ throw
    $ "Test-Suite specification \""
      <> T.pack testSuite
      <> "\" does not exist"
