module Fpex.Summary (gradeReport, gradeTestGroup, scoreReport, scoreGroup) where

import Fpex.Types
import           Data.Coerce                  ( coerce )
import           Data.List                      ( foldl' )

scoreGroup :: TestGroup (TestCase, TestCaseResult) -> TestSummary -> Points
scoreGroup TestGroup { penalty, pointsPerTest } TestSummary { failedTest, okTest }
    = coerce okTest * pointsPerTest - penalty * coerce failedTest

scoreReport :: TestReport -> Points
scoreReport (TestReport report) =
    sum $ map (\rep -> scoreGroup rep $ gradeTestGroup rep) report

gradeTestGroup :: TestGroup (TestCase, TestCaseResult) -> TestSummary
gradeTestGroup TestGroup { group } =
    foldl' (\acc c -> acc <> uncurry gradedPoints c) mempty group

gradedPoints :: TestCase -> TestCaseResult -> TestSummary
gradedPoints _ TestCaseCompilefail  = testCompileFail <> testFailed
gradedPoints _ TestCaseNotSubmitted = testNotSubmitted <> testFailed
gradedPoints _ TestCaseTimeout      = testTimeOut <> testFailed
gradedPoints TestCase { expectedOutput } (TestCaseRun TestRun { actualOutput })
    = if expectedOutput == actualOutput then testOk else testFailed

gradeReport :: TestReport -> TestSummary
gradeReport (TestReport report) = mconcat $ map gradeTestGroup report
