module Fpex.Grade where

import qualified Data.Aeson                    as Aeson
import qualified System.Process                as Proc
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Exit                    ( ExitCode(..) )
import           System.Directory
import           System.FilePath                ( dropExtension
                                                , takeFileName
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
    -> Course
    -> String
    -> Student
    -> Sem r ()
runSubmission sid course testSuite student = do
    let targetDir  = assignmentCollectStudentDir sid course testSuite student
    let targetFile = assignmentCollectStudentFile sid course testSuite student

    ghciOptions <- asks courseGhciOptions
    ghciEnv     <- asks ghciEnvironmentLocation

    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission

    procRes <- embed $ do
        T.putStrLn $ "run testsuite for student " <> matrNr student
        let procConfig = (  Proc.proc "ghci"
                         $  [ "../Main.hs"
                            , "-package-env"
                            , ghciEnv
                            , "-i"
                            , "-i."
                            , "-i.."
                            , "-e"
                            , "Main.main"
                            ]
                         ++ ghciOptions
                         ) { Proc.cwd = Just targetDir
                           }
        (_, _, _, procHandle) <- Proc.createProcess procConfig
        Proc.waitForProcess procHandle

    case procRes of
        ExitSuccess   -> return ()
        ExitFailure _ -> throw RunnerFailedToCompile

-- | Create a directory
createEmptyStudent
    :: Members '[Embed IO, Error RunnerError, Reader Course] r
    => SubmissionId
    -> Course
    -> FilePath
    -> Sem r (Eval.TestSuiteResults, Eval.TestSuiteResults)
createEmptyStudent sid course testsuite = do
    let errorStudent = Student "errorStudent"
    let targetDir =
            Eval.assignmentCollectStudentDir sid course testsuite errorStudent
    let targetFile =
            Eval.assignmentCollectStudentFile sid course testsuite errorStudent
    let moduleName = T.pack $ dropExtension $ takeFileName testsuite
    let resultSourceFile =
            Eval.reportSourceJsonFile sid course testsuite errorStudent
    embed $ createDirectoryIfMissing True targetDir
    embed $ T.writeFile targetFile ("module " <> moduleName <> " where\n")

    runSubmission sid course testsuite errorStudent

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
