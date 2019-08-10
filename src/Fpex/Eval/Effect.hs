module Fpex.Eval.Effect where

import           Polysemy
import           Polysemy.Internal
import qualified Data.Text                     as T

import           System.Process                 ( readProcessWithExitCode )
import           System.Exit                    ( ExitCode(..) )

import           Fpex.Eval.Types

data Grade m a where
    RunTestCase ::FilePath -> TestCase -> Grade m TestCaseResult

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


gradeTestCase :: Member Grade r => FilePath -> TestCase -> Sem r TestCaseResult
gradeTestCase fp testCase = send (RunTestCase fp testCase)
