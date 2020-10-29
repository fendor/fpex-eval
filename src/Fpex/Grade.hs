module Fpex.Grade where

import Colog.Polysemy
import qualified Colog.Polysemy as Log
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as T
import Fpex.Course.Types
import Fpex.Grade.Paths
import Fpex.Grade.Storage
import Fpex.Grade.Types
import Fpex.Grade.Result as Eval
import Polysemy
import Polysemy.Error (Error, catch, runError, throw)
import Polysemy.Internal
import Polysemy.Reader
import System.Directory

data RunnerError
  = RunnerInternalError T.Text LBS.ByteString
  | FailedToDecodeJsonResult String
  | NoSubmission
  deriving (Show, Eq, Read, Ord)

runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

data RunTestSuite m a where
  -- | Generates a hash from a password
  RunTestSuite :: SubmissionInfo -> RunTestSuite m TestSuiteResults

runTestSuite :: Member RunTestSuite r => SubmissionInfo -> Sem r TestSuiteResults
runTestSuite s = send $ RunTestSuite s

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
      Storage
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
  studentSubmission <- asks runnerInfoStudentSubmission
  let targetFile =
        assignmentCollectStudentFile subId subName studentSubmission subStudent
  embed $ copyFile baseDefinitions targetFile
  testSuiteResults <- runTestSuite sinfo
  let notSubmitted = NotSubmittedReport $ setTestSuiteResults Eval.TestCaseResultNotSubmitted testSuiteResults
  let compileFail = CompileFailReport $ setTestSuiteResults Eval.TestCaseResultCompileFail testSuiteResults
  return $ newErrorReports compileFail notSubmitted
  where
    setTestCaseResult res report =
      report {testCaseReportResult = res}

    setTestGroupResult res Eval.TestGroupResults {..} =
      Eval.TestGroupResults
        { testGroupReports = map (setTestCaseResult res) testGroupReports,
          ..
        }

    setTestSuiteResults res Eval.TestSuiteResults {..} =
      Eval.TestSuiteResults
        { testGroupResults = map (setTestGroupResult res) testGroupResults,
          ..
        }