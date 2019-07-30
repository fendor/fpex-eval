-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import           Test.Tasty.Hspec
import           Fpex.EvalMain
import           Fpex.Types
import qualified Data.Text                     as T
import           Control.Monad                  ( forM_ )

main :: IO ()
main = do
    test <- testSpec "fpex-eval" spec
    Test.Tasty.defaultMain test

data Submission = Submission
    { student :: Student
    , testReport :: TestReport
    , points :: Int
    }

testSuiteSimple :: TestSuite
testSuiteSimple = TestSuite testCasesSimple

testCasesSimple :: [TestCase]
testCasesSimple =
    [ TestCase "fib 0"        "1"       5
    , TestCase "fib 1"        "1"       5
    , TestCase "fib 2"        "2"       5
    , TestCase "fib 3"        "3"       5
    , TestCase "fib 4"        "5"       5
    , TestCase "fib 5"        "8"       5
    , TestCase "factorial 3"  "6"       5
    , TestCase "factorial 0"  "1"       5
    , TestCase "factorial 10" "3628800" 10
    ]

submissionsSimple :: [Submission]
submissionsSimple =
    [ Submission -- ^ Everything correct
        { student    = Student "1234567"
        , testReport = TestReport $ zip
                           testCasesSimple
                           [ TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "2"
                           , TestCaseRun $ TestRun "3"
                           , TestCaseRun $ TestRun "5"
                           , TestCaseRun $ TestRun "8"
                           , TestCaseRun $ TestRun "6"
                           , TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "3628800"
                           ]
        , points     = 50
        }
    , Submission -- ^ Fibonacci sequence incorrectly defined as f_(n+2) = f_n + f_n
        -- Also, no submission for the function "factorial"
        { student    = Student "1000000"
        , testReport = TestReport $ zip
                           testCasesSimple
                           [ TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "2"
                           , TestCaseRun $ TestRun "2"
                           , TestCaseRun $ TestRun "4"
                           , TestCaseRun $ TestRun "4"
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           ]
        , points     = 15
        }
    , Submission -- ^ Fibonacci sequence incorrectly defined as f_(n+2) = f_n + f_n
        { student    = Student "1711000"
        , testReport = TestReport $ zip
                           testCasesSimple
                           [ TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "2"
                           , TestCaseRun $ TestRun "2"
                           , TestCaseRun $ TestRun "4"
                           , TestCaseRun $ TestRun "4"
                           , TestCaseRun $ TestRun "6"
                           , TestCaseRun $ TestRun "1"
                           , TestCaseRun $ TestRun "3628800"
                           ]
        , points     = 35
        }
    , Submission -- ^ Submission contains a typo. Thus, everything fails
        { student    = Student "1831000"
        , testReport = TestReport $ zip
                           testCasesSimple
                           [ TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           , TestCaseCompilefail
                           ]
        , points     = 0
        }
    ]

spec :: Spec
spec =
    parallel
        $ describe "evaluate student, all points"
        $ forM_ submissionsSimple
        $ \submission ->
              describe
                      (  "submission of student"
                      <> T.unpack (matrNr $ student submission)
                      )
                  $ do

                        it "report should be correct" $ do
                            report <- evalStudent testSuiteSimple
                                                  (student submission)
                            report `shouldBe` testReport submission

                        it "points should be correct" $ do
                            report <- evalStudent testSuiteSimple
                                                  (student submission)
                            receivedPoints report `shouldBe` points submission
