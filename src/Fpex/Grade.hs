module Fpex.Grade where

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString.Lazy          as LBS
import           Data.Function
import qualified System.Process.Typed          as Proc
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit                    ( ExitCode(..) )
import           System.Directory
import           System.FilePath                ( (</>)
                                                )
import           Control.Monad.Extra            ( unlessM )

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Fpex.Course.Types
import           Fpex.Eval.Types               as Eval

data RunnerError
    = RunnerFailedToCompile
    | NoSubmission
    deriving (Show, Eq, Read, Ord)

runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

runSubmission
    :: Members '[Embed IO, Error RunnerError, Reader Course] r
    => SubmissionId
    -> T.Text
    -> Student
    -> Sem r ()
runSubmission sid suiteName student = do
    let targetDir  = assignmentCollectStudentDir sid suiteName student
    let targetFile = assignmentCollectStudentFile sid suiteName student

    ghciOptions <- asks courseGhciOptions
    ghciEnv'    <- asks ghciEnvironmentLocation
    ghciEnv     <- embed $ makeAbsolute ghciEnv'

    embed $ T.putStrLn $ "run testsuite for student " <> studentId student
    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission
    procRes <- embed $ do
        let procArgs =  [ "../Main.hs"
                        , "-package-env"
                        , ghciEnv
                        , "-i"
                        , "-i."
                        , "-i.."
                        , "-e"
                        , "Main.main"
                        ]
                        ++ ghciOptions
        let procConfig = Proc.proc "ghci" procArgs
                        & Proc.setWorkingDir targetDir
        (r, sout, serr) <- Proc.readProcess procConfig
        LBS.writeFile (targetDir </> "report.json") sout
        LBS.writeFile (targetDir </> "stderr.log") serr
        return r

    case procRes of
        ExitSuccess   -> return ()
        ExitFailure _ -> throw RunnerFailedToCompile

-- | Create a directory
createEmptyStudent
    :: Members '[Embed IO, Error RunnerError, Reader Course] r
    => SubmissionId
    -> T.Text
    -> Sem r (Eval.TestSuiteResults, Eval.TestSuiteResults)
createEmptyStudent sid suiteName = do
    let errorStudent = Student "errorStudent"
    let targetDir =
            Eval.assignmentCollectStudentDir sid suiteName errorStudent
    let targetFile =
            Eval.assignmentCollectStudentFile sid suiteName errorStudent
    let resultSourceFile =
            Eval.reportSourceJsonFile sid suiteName errorStudent
    embed $ createDirectoryIfMissing True targetDir
    embed $ T.writeFile targetFile ("module " <> suiteName <> " where\n")

    runSubmission sid suiteName errorStudent

    testSuiteResults <-
        embed (Aeson.eitherDecodeFileStrict resultSourceFile) >>= \case
            Right (r :: Eval.TestSuiteResults) -> return r
            Left  _msg                         -> throw RunnerFailedToCompile

    let modifyTestCaseResult report =
            report { testCaseReportResult = Eval.TestCaseResultCompileFail }
        modifyTestGroup Eval.TestGroupResults {..} = Eval.TestGroupResults
            { testGroupReports = map modifyTestCaseResult testGroupReports
            , ..
            }
        modifyTestSuiteResults Eval.TestSuiteResults {..} =
            Eval.TestSuiteResults
                { testGroupResults = map modifyTestGroup testGroupResults
                , ..
                }
    return ((modifyTestSuiteResults testSuiteResults), testSuiteResults)
