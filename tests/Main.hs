-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import Fpex.EvalMain
import Fpex.Types

main :: IO ()
main = do
    test <- testSpec "fpex-eval" spec
    Test.Tasty.defaultMain test

studentSimple = Student "1234567"

testSuiteSimple =
    TestSuite
        [ TestCase "fib 0" "1" 5
        , TestCase "fib 1" "1" 5
        , TestCase "fib 2" "2" 5
        , TestCase "fib 3" "3" 5
        , TestCase "fib 4" "5" 5
        , TestCase "fib 5" "8" 5
        , TestCase "factorial 3" "6" 5
        , TestCase "factorial 0" "1" 5
        , TestCase "factorial 10" "3628800" 10
        ]

testReportSimple =
    TestReport
        [ (TestCase "fib 0" "1" 5, TestCaseRun $ TestRun "1")
        , (TestCase "fib 1" "1" 5, TestCaseRun $ TestRun"1")
        , (TestCase "fib 2" "2" 5, TestCaseRun $ TestRun"2")
        , (TestCase "fib 3" "3" 5, TestCaseRun $ TestRun"3")
        , (TestCase "fib 4" "5" 5, TestCaseRun $ TestRun"5")
        , (TestCase "fib 5" "8" 5, TestCaseRun $ TestRun"8")
        , (TestCase "factorial 3" "6" 5, TestCaseRun $ TestRun "6")
        , (TestCase "factorial 0" "1" 5, TestCaseRun $ TestRun "1")
        , (TestCase "factorial 10" "3628800" 10, TestCaseRun $ TestRun "3628800")
        ]

spec :: Spec
spec = parallel $ do
    it "evaluate student, all points" $ do
        report <- evalStudent testSuiteSimple studentSimple
        report `shouldBe` testReportSimple