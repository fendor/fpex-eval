module Fpex.Eval.Summary
    ( gradeReport
    , gradeTestGroup
    , scoreReport
    , scoreGroup
    )
where

import           Fpex.Eval.Types
import           Data.Coerce                    ( coerce )
import           Data.List                      ( foldl' )

scoreGroup :: TestGroup (TestCase, TestCaseResult) -> Points
scoreGroup t@TestGroup { penalty, pointsPerTest, maximal } = max
    (min points maximal)
    0
  where
    TestSummary { okTest, failedTest } = gradeTestGroup t
    points = coerce okTest * pointsPerTest - penalty * coerce failedTest

scoreReport :: TestReport -> Points
scoreReport (TestReport report) = sum $ map scoreGroup report

gradeTestGroup :: TestGroup (TestCase, TestCaseResult) -> TestSummary
gradeTestGroup TestGroup { group } =
    foldl' (\acc c -> acc <> uncurry gradedPoints c) mempty group

gradedPoints :: TestCase -> TestCaseResult -> TestSummary
gradedPoints _ TestCaseCompilefail  = testCompileFail <> testFailed
gradedPoints _ TestCaseNotSubmitted = testNotSubmitted <> testFailed
gradedPoints _ TestCaseTimeout      = testTimeOut <> testFailed
gradedPoints _ TestRunTimeException = testRunTimeException <> testFailed
gradedPoints TestCase { expectedOutput } (TestCaseRun TestRun { actualOutput })
    = if expectedOutput == actualOutput then testOk else testFailed

gradeReport :: TestReport -> TestSummary
gradeReport (TestReport report) = mconcat $ map gradeTestGroup report
