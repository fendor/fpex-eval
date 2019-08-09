module Fpex.Eval.Main where

import           Options.Applicative
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as L
import qualified Data.Text.IO                  as T
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Aeson                     ( decodeFileStrict' )
import           Control.Monad.IO.Class
import           Fpex.Eval.Options
import           Fpex.Eval.Pretty
import           Fpex.Eval.Types
import qualified Fpex.User.Simple              as User
import qualified Fpex.User.Types               as User
import           Control.Monad.Extra            ( whenJust )
import           Control.Monad                  ( forM )
import           System.Process                 ( readProcessWithExitCode )
import           System.Directory               ( doesFileExist )
import           System.FilePath                ( (</>) )
import           System.Exit                    ( ExitCode(..) )

defaultMain :: IO ()
defaultMain = execParser options >>= \case
    Eval CommandGrade {..} -> do
        Just testSuite <- decodeFileStrict' testSuiteFile
        testReport     <- evalStudent testSuite student
        putStrLn . L.unpack . encodeToLazyText $ testReport

    User UserManagementCommand {..} ->
        
        User.createUser username userGroup >>= \case
            Left err -> print err

            Right User.Password { getPassword = password } ->
                T.putStrLn password

evalStudent :: TestSuite -> Student -> IO TestReport
evalStudent (TestSuite testGroups) student = studentFile student >>= \case
    -- If no file can be found, mark anything as not submitted.
    Nothing -> return $ TestReport
        (map
            (\testGroup@TestGroup { group } ->
                testGroup { group = zip group (repeat TestCaseNotSubmitted) }
            )
            testGroups
        )
    -- run the test cases only if there is a submission.
    Just fp -> do
        testResults <- mapM (runTestGroup fp) testGroups
        return $ TestReport testResults

runTestGroup
    :: FilePath
    -> TestGroup TestCase
    -> IO (TestGroup (TestCase, TestCaseResult))
runTestGroup fp testGroup@TestGroup { group } = do
    results <- forM group (runTest fp)
    return $ testGroup { group = zip group results }

runTest :: FilePath -> TestCase -> IO TestCaseResult
runTest fp TestCase { query } = do
    -- TODO: generalise this
    -- TODO: look at stderr to differentiate between compile error and not submitted
    (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode
        "timeout"
        ["3", "ghci", fp, "-e", T.unpack query]
        []
    case exitCode of
        ExitFailure 124 -> return TestCaseTimeout
        ExitFailure _   -> return TestCaseCompilefail
        ExitSuccess     -> do
            let actualOutput = T.strip $ T.pack stdout
            return . TestCaseRun $ TestRun actualOutput

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
