module Fpex.Grade where

import Control.Monad.Extra (unlessM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

prettyTestReport :: Members [Embed IO, Reader SubmissionInfo, Reader ErrorReports] r => Either RunnerError TestSuiteResults -> Sem r ()
prettyTestReport (Right testResult) = do
  embed $ T.putStrLn $
    T.unlines $ map ("\t" <>)
      [ "Test Report:",
        "",
        T.concat
          [ "Points: ",
            T.pack (show $ Eval.testSuitePoints testResult),
            "/",
            T.pack (show $ Eval.maxScore testResult)
          ],
        "",
        "Correct:       " <> (T.pack . show $ Eval.correctTests testResult),
        "Incorrect:     " <> (T.pack . show $ Eval.failedTests testResult),
        "Not submitted: " <> (T.pack . show $ Eval.notSubmittedTests testResult),
        "Timeout:       " <> (T.pack . show $ Eval.timeoutTests testResult)
      ]
prettyTestReport (Left err) = do
  embed $ T.putStr "\t"
  case err of
    RunnerError _ serr -> do
      embed $ T.putStrLn "Runner Error"
      embed $ LBS.putStrLn serr
      CompileFailReport compileFailTestSuite <- asks compileFailReport
      writeTestSuiteResult compileFailTestSuite
    FailedToDecodeJsonResult msg ->
      embed $ T.putStrLn $ T.pack msg
    NoSubmission -> do
      embed $ T.putStrLn "No Submission"
      NotSubmittedReport noSubmissionTestSuite <- asks notSubmittedReport
      writeTestSuiteResult noSubmissionTestSuite

runSubmission ::
  Members '[Embed IO, Error RunnerError, Reader Course, Reader SubmissionInfo] r =>
  Sem r Eval.TestSuiteResults
runSubmission = do
  sid <- asks subId
  suiteName <- asks subTestSuite
  student <- asks subStudent
  let targetDir = assignmentCollectStudentDir sid suiteName student
  let targetFile = assignmentCollectStudentFile sid suiteName student
  ghciOptions <- asks courseGhciOptions
  ghciEnv <- asks ghciEnvironmentLocation
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
      writeTestSuiteResult s
      pure s

-- | Create a directory
createEmptyStudent ::
  Members '[Embed IO, Error RunnerError, Reader Course] r =>
  SubmissionId ->
  T.Text ->
  Sem r Eval.ErrorReports
createEmptyStudent sid suiteName = do
  let errorStudent = Student "errorStudent"
  let sinfo =
        SubmissionInfo
          { subId = sid,
            subTestSuite = suiteName,
            subStudent = errorStudent
          }
  testSuiteResults <- runReader sinfo runSubmission
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
