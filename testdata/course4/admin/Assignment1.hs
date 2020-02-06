
import           TestSpec
import           Test.HUnit                     ( (@?=) )
import           Assignment1

main = do
    result <- runTestSuite 5 $ testSuite
        [ group (TestGroupProps "" 5 0 26)
                [fib 0 @?= 1, fib 1 @?= 1, fib 2 @?= 2, fib 3 @?= 3, fib 4 @?= 5]
        ]
    writeTestSuiteResults "results.out" result
