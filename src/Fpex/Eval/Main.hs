module Fpex.Eval.Main where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

import           Control.Monad.Extra            ( whenJust )
import           Control.Monad                  ( forM )

import           Polysemy

import           System.Directory               ( doesFileExist )
import           System.FilePath                ( (</>) )

import           Fpex.Eval.Pretty
import           Fpex.Eval.Types
import           Fpex.Eval.Effect
import           Fpex.Course.Types


evalStudent
    :: Members '[Embed IO, Grade] r => TestSuite -> Student -> Sem r TestReport
evalStudent (TestSuite { testSuiteGroups }) student = studentFile student >>= \case
    -- If no file can be found, mark anything as not submitted.
    Nothing -> return $ TestReport
        (map
            (\testGroup@TestGroup { group } ->
                testGroup { group = zip group (repeat TestCaseNotSubmitted) }
            )
            testSuiteGroups
        )
    -- run the test cases only if there is a submission.
    Just fp -> do
        testResults <- mapM (runTestGroup fp) testSuiteGroups
        return $ TestReport testResults

runTestGroup
    :: Members '[Embed IO, Grade] r
    => FilePath
    -> TestGroup TestCase
    -> Sem r (TestGroup (TestCase, TestCaseResult))
runTestGroup fp testGroup@TestGroup { group } = do
    results <- forM group (gradeTestCase fp)
    return $ testGroup { group = zip group results }

-- TODO: this needs to go into some config effect
studentFile :: Member (Embed IO) r => Student -> Sem r (Maybe FilePath)
studentFile Student { matrNr } = do
    let fp = "testdata" </> "student-" <> T.unpack matrNr <> ".hs"
    embed (doesFileExist fp) >>= \case
        True  -> pure $ Just fp
        False -> pure Nothing

generateReport
    :: Member (Embed IO) r => Student -> TestReport -> FilePath -> Sem r ()
generateReport _ report _ = embed . T.putStrLn $ prettyTestReport report

grade :: Members '[Embed IO, Grade] r => TestSuite -> Student -> Sem r ()
grade testsuite student = do
    report  <- evalStudent testsuite student
    fileMay <- studentFile student
    whenJust fileMay $ \fp -> generateReport student report fp
