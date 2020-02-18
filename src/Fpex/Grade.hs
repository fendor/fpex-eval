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
import           Fpex.Course.Types
import           Fpex.Eval.Types               as Eval
import           Fpex.Publish.Types            as Publish

data RunnerError
    = RunnerFailedToCompile
    | NoSubmission
    deriving (Show, Eq, Read, Ord)

runGradeError :: Sem (Error RunnerError ': r) a -> Sem r (Either RunnerError a)
runGradeError = runError

runSubmission :: (Member (Embed IO) r, Member (Error RunnerError) r) => SubmissionId -> Course -> String -> Student -> Sem r ()
runSubmission sid course testSuite student = do
    let targetDir = assignmentCollectStudentDir sid course testSuite student
    let targetFile = assignmentCollectStudentFile sid course testSuite student

    unlessM (embed $ doesFileExist targetFile) $ throw NoSubmission

    procRes <- embed $ do
        T.putStrLn $ "run testsuite for student " <> matrNr student
        let
            procConfig =
                (Proc.proc "ghci"
                           ["../Main.hs", "-i", "-i.", "-i..", "-e", "Main.main"]
                    )
                    { Proc.cwd = Just targetDir
                    }
        (_, _, _, procHandle) <- Proc.createProcess procConfig
        Proc.waitForProcess procHandle

    case procRes of
        ExitSuccess -> return ()
        ExitFailure _ -> throw RunnerFailedToCompile

-- | Create a directory
createEmptyStudent :: (Member (Embed IO) r, Member (Error RunnerError) r) => SubmissionId -> Course -> FilePath -> Sem r (Publish.TestSuiteResults, Publish.TestSuiteResults)
createEmptyStudent sid course testsuite = do
    let errorStudent = Student "errorStudent"
    let targetDir = Eval.assignmentCollectStudentDir sid course testsuite errorStudent
    let targetFile = Eval.assignmentCollectStudentFile sid course testsuite errorStudent
    let moduleName = T.pack $ dropExtension $ takeFileName testsuite
    let resultSourceFile = Eval.reportSourceJsonFile sid course testsuite errorStudent
    embed $ createDirectoryIfMissing True targetDir
    embed $ T.writeFile targetFile ("module " <> moduleName <> " where\n")

    runSubmission sid course testsuite errorStudent

    testSuiteResults <- embed (Aeson.eitherDecodeFileStrict resultSourceFile)
        >>= \case
        Right (r :: Publish.TestSuiteResults) -> return r
        Left _msg -> throw RunnerFailedToCompile

    let modifyTestCaseResult report =
            report
                { testCaseReportResult = Publish.TestCaseResultCompileFail
                }
        modifyTestGroup  Publish.TestGroupResults {..} =
            Publish.TestGroupResults
                { testGroupReports = map modifyTestCaseResult testGroupReports
                , ..
                }
        modifyTestSuiteResults Publish.TestSuiteResults {..} =
            Publish.TestSuiteResults
                { testGroupResults = map modifyTestGroup testGroupResults
                , ..
                }
    return ((modifyTestSuiteResults testSuiteResults), testSuiteResults)