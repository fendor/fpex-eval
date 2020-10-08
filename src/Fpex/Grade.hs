module Fpex.Grade where

import Colog.Polysemy
import qualified Colog.Polysemy as Log
import Control.Monad.Extra (unlessM)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as T
import Fpex.Course.Types
import Fpex.Grade.Storage
import Fpex.Grade.Tasty as Eval
import Fpex.Grade.Types as Eval
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.Reader
import System.Directory
import System.FilePath
  ( (</>),
  )
import qualified System.Process.Typed as Proc

data RunnerError
  = RunnerInternalError T.Text LBS.ByteString
  | FailedToDecodeJsonResult String
  | NoSubmission
  deriving (Show, Eq, Read, Ord)

runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

prettyRunnerError :: SubmissionInfo -> RunnerError -> Sem r T.Text
prettyRunnerError _sinfo = \case
  RunnerInternalError msg serr ->
    pure $
      T.unlines $
        [ "Failed to execute neutral student",
          msg,
          "Stderr: ",
          T.pack $ LBS.unpack serr
        ]
  FailedToDecodeJsonResult msg ->
    pure $
      T.unlines
        ["Failed to decode the json result: ", T.pack msg]
  NoSubmission ->
    pure "Main.hs:Grade (NoSubmission) Invariant violated, can not be generated here."

data RunTestSuite m a where
  -- | Generates a hash from a password
  RunTestSuite :: SubmissionInfo -> RunTestSuite m TestSuiteResults

runTestSuite :: Member RunTestSuite r => SubmissionInfo -> Sem r TestSuiteResults
runTestSuite s = send $ RunTestSuite s

runTastyTestSuite ::
  Members
    [ Error RunnerError,
      Embed IO,
      Reader Course,
      Reader RunnerInfo
    ]
    r =>
  Sem (RunTestSuite : r) a ->
  Sem r a
runTastyTestSuite = interpret $ \case
  RunTestSuite SubmissionInfo {..} -> do
    let sid = subId
    let suiteName = subTestSuite
    let student = subStudent
    let targetDir = assignmentCollectStudentDir sid suiteName student
    let targetFile = assignmentCollectStudentFile sid suiteName student
    ghciOptions <- asks courseGhciOptions
    ghciEnv <- asks ghciEnvironmentLocation
    reportOutput <- asks runnerInfoReportOutput
    testTimeout <- asks runnerInfoTimeout
    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission
    let procArgs =
          [ "../Main.hs",
            "-package-env",
            ghciEnv,
            "-i",
            "-i.",
            "-i..",
            "-e",
            unwords
              [ ":main",
                "-j",
                "1",
                "-t",
                show (getTimeout testTimeout),
                "--grading-json",
                reportOutput
              ]
          ]
            ++ ghciOptions
        procConfig =
          Proc.proc "ghci" procArgs
            & Proc.setWorkingDir targetDir
    (_procRes, sout, serr) <- Proc.readProcess procConfig
    -- Write logs to files
    embed $ do
      LBS.writeFile (targetDir </> "stderr.log") serr
      LBS.writeFile (targetDir </> "stdout.log") sout
    -- case procRes of
    --   ExitSuccess -> return ()
    --   ExitFailure _ -> throw $ RunnerInternalError (T.pack $ show procConfig) serr
    decodeResult <- embed $ decodeFileTastyGradingReport (targetDir </> reportOutput)
    case decodeResult of
      Left msg -> throw $ FailedToDecodeJsonResult msg
      Right s -> pure s

runSubmission ::
  Members
    [ Log T.Text,
      Error T.Text,
      Error RunnerError,
      Reader ErrorReports,
      Reader SubmissionInfo,
      Reader Course,
      Reader RunnerInfo,
      RunTestSuite,
      TestSuiteStorage
    ]
    r =>
  Sem r Eval.TestSuiteResults
runSubmission = do
  submissionInfo <- ask @SubmissionInfo
  testSuiteResult <-
    runTestSuite submissionInfo
      `catch` \case
        (RunnerInternalError _ serr) -> do
          Log.log $ TL.toStrict $ T.decodeUtf8 serr
          CompileFailReport compileFailTestSuite <- asks compileFailReport
          pure compileFailTestSuite
        NoSubmission -> do
          NotSubmittedReport noSubmissionTestSuite <- asks notSubmittedReport
          pure noSubmissionTestSuite
        FailedToDecodeJsonResult msg ->
          throw $ T.pack msg
  writeTestSuiteResult submissionInfo testSuiteResult
  pure testSuiteResult

-- | Create a directory
createEmptyStudent ::
  Members
    [ Embed IO,
      Error RunnerError,
      Reader RunnerInfo,
      Reader Course,
      RunTestSuite
    ]
    r =>
  FilePath ->
  SubmissionInfo ->
  Sem r Eval.ErrorReports
createEmptyStudent baseDefinitions sinfo@SubmissionInfo {..} = do
  let targetFile =
        assignmentCollectStudentFile subId subTestSuite subStudent
  embed $ copyFile baseDefinitions targetFile
  testSuiteResults <- runTestSuite sinfo
  let modifyTestCaseResult report =
        report {testCaseReportResult = Eval.TestCaseResultCompileFail}
      modifyTestGroup Eval.TestGroupResults {..} =
        Eval.TestGroupResults
          { testGroupReports = map modifyTestCaseResult testGroupReports,
            ..
          }
      modifyTestSuiteResults Eval.TestSuiteResults {..} =
        Eval.TestSuiteResults
          { testGroupResults = map modifyTestGroup testGroupResults,
            ..
          }
  let notSubmitted = NotSubmittedReport testSuiteResults
  let compileFail = CompileFailReport $ modifyTestSuiteResults testSuiteResults
  return $ newErrorReports compileFail notSubmitted
