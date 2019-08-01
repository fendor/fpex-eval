module Fpex.Pretty
    ( prettyTestReport
    )
where

import           Fpex.Types
import           Fpex.Summary
import           Data.Coerce                    ( coerce )
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
renderTestGroup testGroup@TestGroup { label, group, pointsPerTest, penalty } =
    let TestSummary { okTest, failedTest } = gradeTestGroup testGroup
    in  T.unlines
            $  [ label
               , "Points per test case: "
               <> T.pack (show $ getPoints pointsPerTest)
               <> "; penalty per failed test case: "
               <> T.pack (show $ getPoints penalty)
               <> "; maximum: "
               <> T.pack (show $ length group * coerce pointsPerTest)
               ]
            <> [""]
            <> map (uncurry renderTestCase) group
            <> [""]
            <> [ "OK tests: "
               <> T.pack (show $ getOkTest okTest)
               <> "; FAILED tests: "
               <> T.pack (show $ getFailedTest failedTest)
               , "Score: " <> T.pack
                   ( show
                   . getPoints
                   $ (coerce okTest * pointsPerTest)
                   - (coerce penalty * coerce getFailedTest failedTest)
                   )
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

