module Fpex.Grade where

import Control.Monad.Extra (unlessM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import qualified Data.Text as T
import Fpex.Course.Types
import Fpex.Eval.Types as Eval
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
  ( (</>),
  )
import qualified System.Process.Typed as Proc

data RunnerError
  = RunnerError T.Text LBS.ByteString
  | FailedToDecodeJsonResult String
  | NoSubmission
  deriving (Show, Eq, Read, Ord)

runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

runSubmission ::
  Members '[Embed IO, Error RunnerError, Reader Course] r =>
  SubmissionId ->
  T.Text ->
  Student ->
  Sem r Eval.TestSuiteResults
runSubmission sid suiteName student = do
  let targetDir = assignmentCollectStudentDir sid suiteName student
  let targetFile = assignmentCollectStudentFile sid suiteName student
  ghciOptions <- asks courseGhciOptions
  ghciEnv' <- asks ghciEnvironmentLocation
  ghciEnv <- embed $ makeAbsolute ghciEnv'
  unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission
  let procArgs =
        [ "../Main.hs",
          "-package-env",
          ghciEnv,
          "-i",
          "-i.",
          "-i..",
          "-e",
          "Main.main"
        ]
          ++ ghciOptions
      procConfig =
        Proc.proc "ghci" procArgs
          & Proc.setWorkingDir targetDir
  (procRes, sout, serr) <- Proc.readProcess procConfig
  embed $ LBS.writeFile (targetDir </> "stderr.log") serr
  case procRes of
    ExitSuccess -> return ()
    ExitFailure _ -> throw $ RunnerError (T.pack $ show procConfig) serr

  case Aeson.eitherDecode sout of
    Left msg -> throw $ FailedToDecodeJsonResult msg
    Right s -> do
      embed $ writeTestSuiteResult sid suiteName student s
      pure s

-- | Create a directory
createEmptyStudent ::
  Members '[Embed IO, Error RunnerError, Reader Course] r =>
  SubmissionId ->
  T.Text ->
  Sem r (Eval.TestSuiteResults, Eval.TestSuiteResults)
createEmptyStudent sid suiteName = do
  let errorStudent = Student "errorStudent"
  testSuiteResults <- runSubmission sid suiteName errorStudent
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
  return ((modifyTestSuiteResults testSuiteResults), testSuiteResults)
