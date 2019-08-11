module Fpex.Eval.Main where

import qualified Data.Text                     as T

import           Control.Monad.Extra            ( whenJust )
import           Control.Monad                  ( forM )

import           Polysemy
import           Polysemy.Output


import           Fpex.Eval.Pretty
import           Fpex.Eval.Types
import           Fpex.Eval.Effect
import           Fpex.Course.Types


evalStudent
    :: Members '[StudentData, Grade] r
    => Course
    -> TestSuite
    -> Student
    -> Sem r TestReport
evalStudent course testSuite@TestSuite { testSuiteGroups } student =
    getStudentSubmission course testSuite student >>= \case
        -- If no file can be found, mark anything as not submitted.
        Nothing -> return $ TestReport
            (map
                (\testGroup@TestGroup { group } -> testGroup
                    { group = zip group (repeat TestCaseNotSubmitted)
                    }
                )
                testSuiteGroups
            )
        -- run the test cases only if there is a submission.
        Just fp -> do
            testResults <- mapM (runTestGroup fp) testSuiteGroups
            return $ TestReport testResults

runTestGroup
    :: Members '[Grade] r
    => FilePath
    -> TestGroup TestCase
    -> Sem r (TestGroup (TestCase, TestCaseResult))
runTestGroup fp testGroup@TestGroup { group } = do
    results <- forM group (gradeTestCase fp)
    return $ testGroup { group = zip group results }


generateReport
    :: Member (Output T.Text) r => Student -> TestReport -> FilePath -> Sem r ()
generateReport _ report _ = output $ prettyTestReport report

grade
    :: Members '[Output T.Text, StudentData, Grade] r
    => Course
    -> TestSuite
    -> Student
    -> Sem r ()
grade course testSuite student = do
    report  <- evalStudent course testSuite student
    fileMay <- getStudentSubmission course testSuite student
    whenJust fileMay $ generateReport student report
