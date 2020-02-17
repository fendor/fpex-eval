module Fpex.Publish.Pretty where 

import Fpex.Publish.Types
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
              <> map (uncurry renderTestCase) testCaseResults
              <> [""]
              <> [ "OK tests: "
                 <> T.pack (show okTestN)
                 <> "; FAILED tests: "
                 <> T.pack (show failedTestN)
                 , "Score: " <> T.pack (show testGroupPoints)
                 ]
    where 
        (okTest, failedTest) = partition ((== TestCaseResultOk) . snd) testCaseResults
        okTestN = length okTest 
        failedTestN = length failedTest

renderTestCase :: T.Text -> TestCaseResult -> T.Text
renderTestCase query TestCaseResultOk = "Test case: " <> query <> " ; test OK"
renderTestCase query TestCaseResultCompileFail = "Test case: " <> query <> " ; test FAILED TO COMPILE"
renderTestCase query TestCaseResultNotSubmitted = "Test case: " <> query <> " ; test NOT SUBMITTED"
renderTestCase query (TestCaseResultExpectedButGot (ExpectedButGot expectedOutput actualOutput))
    = T.intercalate
        "\n"
        [ buildTestCase query <> " FAILED"
        , "Expected:  " <> actualOutput <> ", but got: " <> expectedOutput 
        ]
renderTestCase query TestCaseResultTimeout =
    buildTestCase query <> " TIMED OUT"
renderTestCase query (TestCaseResultException exception) =
    buildTestCase query <> " RUN-TIME EXCEPTION: " <> exception

buildTestCase :: T.Text -> T.Text
buildTestCase query = "Test case: " <> query <> " ; test"