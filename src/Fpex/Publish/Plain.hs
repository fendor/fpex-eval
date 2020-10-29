module Fpex.Publish.Plain where

import           Fpex.Grade.Result
import qualified Data.Text as T
import           Data.List                      ( partition )

prettyTestReport :: TestSuiteResults -> T.Text
prettyTestReport TestSuiteResults {..} =
    T.strip
        $  T.unlines
        $  map renderTestGroup testGroupResults
        <> [ ""
           , "Total Score: " <> T.pack (show testSuitePoints)
           ]

renderTestGroup :: TestGroupResults  -> T.Text
renderTestGroup TestGroupResults { .. }
    = let TestGroupProps { .. } = testGroupResultProps
      in  T.unlines
              $  [label | not $ T.null label]
              <> [ "Points per test case: "
                   <> T.pack (show pointsPerTest)
                   <> "; penalty per failed test case: "
                   <> T.pack (show penalty)
                   <> "; maximum: "
                   <> T.pack (show upperBound)
                 ]
              <> [""]
              <> map renderTestCase testGroupReports
              <> [""]
              <> [ "OK tests: "
                 <> T.pack (show okTestN)
                 <> "; FAILED tests: "
                 <> T.pack (show failedTestN)
                 , "Score: " <> T.pack (show testGroupPoints)
                 ]
    where
        (okTest, failedTest) = partition ((== TestCaseResultOk) . testCaseReportResult) testGroupReports
        okTestN = length okTest
        failedTestN = length failedTest

renderTestCase :: TestCaseReport -> T.Text
renderTestCase TestCaseReport {..} =
    "Test case: " <> testCaseReportLabel <> " ; test " <> renderTestCaseResult testCaseReportResult

renderTestCaseResult :: TestCaseResult -> T.Text
renderTestCaseResult TestCaseResultOk = "OK"
renderTestCaseResult TestCaseResultCompileFail = "FAILED TO COMPILE"
renderTestCaseResult TestCaseResultNotSubmitted = "NOT SUBMITTED"
renderTestCaseResult (TestCaseResultExpectedButGot (ExpectedButGot expectedOutput actualOutput))
    =  "FAILED\n"
    <> "Expected:  " <> T.pack expectedOutput <> ", but got: " <> T.pack actualOutput
renderTestCaseResult TestCaseResultTimeout =
    "TIMED OUT"
renderTestCaseResult (TestCaseResultException exception) =
    "RUN-TIME EXCEPTION: " <> T.pack exception
