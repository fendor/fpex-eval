module Fpex.Eval.Effect where

import           Polysemy
import           Polysemy.Internal
import qualified Data.Text                     as T

import           System.Process                 ( readProcessWithExitCode )
import           System.Exit                    ( ExitCode(..) )
import           System.Directory               ( doesFileExist )

import           Fpex.Course.Types
import           Fpex.Eval.Types

data Grade m a where
    RunTestCase ::FilePath -> TestCase -> Grade m TestCaseResult

data StudentData m a where
    GetStudentSubmission ::Course -> TestSuite -> Student -> StudentData m (Maybe FilePath)

runGrade :: Member (Embed IO) r => Sem (Grade : r) a -> Sem r a
runGrade = interpret $ \case
    RunTestCase fp TestCase { query } -> do
        -- TODO: generalise this
        -- TODO: look at stderr to differentiate between compile error and not submitted
        (exitCode, stdout, _) <- embed $ readProcessWithExitCode
            "timeout"
            ["3", "ghci", fp, "-e", T.unpack query]
            []
        case exitCode of
            ExitFailure 124 -> return TestCaseTimeout
            ExitFailure _   -> return TestCaseCompilefail
            ExitSuccess     -> do
                let actualOutput = T.strip $ T.pack stdout
                return . TestCaseRun $ TestRun actualOutput

runStudentData :: Member (Embed IO) r => Sem (StudentData : r) a -> Sem r a
runStudentData = interpret $ \case
            GetStudentSubmission course testSuite student -> do
                let sourceFile = assignmentCollectFile course testSuite student
                embed (doesFileExist sourceFile) >>= \case
                    True  -> pure $ Just sourceFile
                    False -> pure Nothing

gradeTestCase :: Member Grade r => FilePath -> TestCase -> Sem r TestCaseResult
gradeTestCase fp testCase = send (RunTestCase fp testCase)

getStudentSubmission :: Member StudentData r => Course -> TestSuite -> Student -> Sem r (Maybe FilePath)
getStudentSubmission course suite student = send (GetStudentSubmission course suite student)