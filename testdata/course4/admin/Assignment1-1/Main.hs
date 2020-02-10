{-# LANGUAGE TemplateHaskell #-}
import           TestSpec
import           Assignment1
main =
    runTestSuite "results.out" 5 $ testSuite
        [ group (TestGroupProps "" 5 0 26)
                [ $(testcase [e| fib 0 `assertEqual` 1 |])
                , $(testcase [e| fib 1 `assertEqual` 1 |])
                , $(testcase [e| fib 2 `assertEqual` 2 |])
                , $(testcase [e| fib 3 `assertEqual` 3 |])
                , $(testcase [e| fib 4 `assertEqual` 5 |])
                ]
        ]