module Fpex.Eval.Pretty
    ( prettyTestReport
    )
where

import           Fpex.Eval.Types
import           Fpex.Eval.Summary
import qualified Data.Text                     as T

prettyTestReport :: TestReport -> T.Text
prettyTestReport report@(TestReport results) =
    T.strip
        $  T.unlines
        $  map renderTestGroup results
        <> [ ""
           , "Total Score: " <> T.pack (show . getPoints $ scoreReport report)
           ]

renderTestGroup :: TestGroup (TestCase, TestCaseResult) -> T.Text
renderTestGroup testGroup@TestGroup { label, group, pointsPerTest, penalty, maximal }
    = let TestSummary { okTest, failedTest } = gradeTestGroup testGroup
      in  T.unlines
              $  [label | not $ T.null label]
              <> [ "Points per test case: "
                   <> T.pack (show $ getPoints pointsPerTest)
                   <> "; penalty per failed test case: "
                   <> T.pack (show $ getPoints penalty)
                   <> "; maximum: "
                   <> T.pack (show $ getPoints maximal)
                 ]
              <> [""]
              <> map (uncurry renderTestCase) group
              <> [""]
              <> [ "OK tests: "
                 <> T.pack (show $ getOkTest okTest)
                 <> "; FAILED tests: "
                 <> T.pack (show $ getFailedTest failedTest)
                 , "Score: " <> T.pack (show . getPoints $ scoreGroup testGroup)
                 ]

renderTestCase :: TestCase -> TestCaseResult -> T.Text
renderTestCase TestCase { expectedOutput, query } (TestCaseRun TestRun { actualOutput })
    | expectedOutput == actualOutput
    = "Test case: " <> query <> " == " <> expectedOutput <> "; test OK"
    | otherwise
    = T.intercalate
        "\n"
        [ "Test case: " <> query <> " ; test FAILED"
        , "Expected:  " <> query <> " == " <> expectedOutput
        , "Result:    " <> query <> " == " <> actualOutput
        ]
renderTestCase TestCase { query } TestCaseNotSubmitted =
    "Test case: " <> query <> " ; test NOT SUBMITTED"
renderTestCase TestCase { query } TestCaseCompilefail =
    "Test case: " <> query <> " ; test FAILED COMPILATION"
renderTestCase TestCase { query } TestCaseTimeout =
    "Test case: " <> query <> " ; test TIMED OUT"
renderTestCase TestCase { query } TestRunTimeException =
    "Test case: " <> query <> " ; test RUN-TIME EXCEPTION"

