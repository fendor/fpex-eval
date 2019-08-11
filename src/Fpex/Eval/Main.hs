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
    :: Members '[Embed IO, Grade] r => Course -> TestSuite -> Student -> Sem r TestReport
evalStudent course testSuite@TestSuite { testSuiteGroups } student =
    studentFile course testSuite student >>= \case
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
studentFile :: Member (Embed IO) r => Course -> TestSuite -> Student -> Sem r (Maybe FilePath)
studentFile course testSuite student = do
    let sourceFile = assignmentCollectFile course testSuite student
    embed (doesFileExist sourceFile) >>= \case
        True  -> pure $ Just sourceFile
        False -> pure Nothing

generateReport
    :: Member (Embed IO) r => Student -> TestReport -> FilePath -> Sem r ()
generateReport _ report _ = embed . T.putStrLn $ prettyTestReport report

grade :: Members '[Embed IO, Grade] r => Course -> TestSuite -> Student -> Sem r ()
grade course testSuite student = do
    report  <- evalStudent course testSuite student
    fileMay <- studentFile course testSuite student
    whenJust fileMay $ generateReport student report
