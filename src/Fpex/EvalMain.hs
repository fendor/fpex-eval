module Fpex.EvalMain where

import           Options.Applicative
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.List                      ( foldl' )
import           Control.Monad.IO.Class
import           Fpex.EvalOptions
import           Fpex.Types
import           Control.Monad.Extra            ( whenJust )
import           Control.Monad                  ( forM )
import           System.Process                 ( readProcessWithExitCode )
import           System.Directory               ( doesFileExist )
import           System.FilePath                ( (</>) )
import           System.Exit                    ( ExitCode(ExitSuccess) )

defaultMain :: IO ()
defaultMain = execParser fpexEvalOptions >>= print

evalStudent :: TestSuite -> Student -> IO TestReport
evalStudent (TestSuite tests) student = do
    testResults <- forM tests $ \test@TestCase { query } ->
        studentFile student >>= \case
            Nothing -> return (test, TestCaseNotSubmitted)
            Just fp -> do
                (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode
                    "ghci"
                    [fp, "-e", T.unpack query]
                    []
                if exitCode /= ExitSuccess
                    then return (test, TestCaseCompilefail)
                    else do
                        let actualOutput = T.strip $ T.pack stdout
                        return (test, TestCaseRun $ TestRun actualOutput)
    return $ TestReport testResults

studentFile :: Student -> IO (Maybe FilePath)
studentFile Student { matrNr } = do
    let fp = "testdata" </> "student-" <> T.unpack matrNr <> ".hs"
    doesFileExist fp >>= \case
        True  -> pure $ Just fp
        False -> pure Nothing


generateReport :: Student -> TestReport -> FilePath -> IO ()
generateReport _ report _ = T.putStrLn $ prettyTestReport report

grade :: TestSuite -> Student -> IO ()
grade testsuite student = do
    report  <- evalStudent testsuite student
    fileMay <- studentFile student
    whenJust fileMay $ \fp -> generateReport student report fp

prettyTestReport :: TestReport -> T.Text
prettyTestReport _ = undefined

gradedPoints :: TestCase -> TestCaseResult -> Int
gradedPoints _ TestCaseCompilefail  = 0
gradedPoints _ TestCaseNotSubmitted = 0
gradedPoints TestCase { expectedOutput, maxPoints } (TestCaseRun TestRun { actualOutput })
    = if expectedOutput == actualOutput then maxPoints else 0

receivedPoints :: TestReport -> Int
receivedPoints (TestReport points) = foldl'
    (\acc (testCase, testResult) -> acc + gradedPoints testCase testResult)
    0
    points
