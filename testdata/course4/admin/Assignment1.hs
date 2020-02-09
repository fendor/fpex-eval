import           TestSpec
import           Assignment1

main = do
    result <- runTestSuite 5 $ testSuite
        [ group
              (TestGroupProps "" 5 0 26)
              [ fib 0 `assertEqual` 1
              , fib 1 `assertEqual` 1
              , fib 2 `assertEqual` 2
              , fib 3 `assertEqual` 3
              , fib 4 `assertEqual` 5
              ]
        ]
    writeTestSuiteResults "results.out" result
