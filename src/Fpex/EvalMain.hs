module Fpex.EvalMain where

import           Options.Applicative
import qualified Data.Text as T
import Control.Monad.IO.Class
import           Fpex.EvalOptions
import           Fpex.Types
import           Control.Monad.Extra            ( whenJust )
import           Control.Monad                  ( forM )
import           System.Process                 ( readProcess )

defaultMain :: IO ()
defaultMain = execParser fpexEvalOptions >>= print

evalStudent :: TestSuite -> Student -> IO TestReport
evalStudent (TestSuite tests) student = do
    testResults <- forM tests $ \test@TestCase {query} ->
        studentFile student >>= \case
            Nothing -> return (test, TestCaseNotSubmitted)
            Just fp -> do
                actualOutput <- liftIO $ readProcess "ghci" [fp, "-e", query] []
                return (test, TestCaseRun $ TestRun actualOutput)
    return $ TestReport testResults

studentFile :: Student -> IO (Maybe FilePath)
studentFile _ = undefined

generateReport :: Student -> TestReport -> FilePath -> IO ()
generateReport _ _ _ = undefined

grade :: TestSuite -> Student -> IO ()
grade testsuite student = do
    report <- evalStudent testsuite student
    fileMay <- studentFile student
    whenJust fileMay $ \fp -> generateReport student report fp

prettyTestReport :: TestReport -> T.Text
prettyTestReport _ = undefined