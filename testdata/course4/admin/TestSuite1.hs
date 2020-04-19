{-# LANGUAGE TemplateHaskell #-}
import qualified TestSpec as T
import           Assignment1 hiding (main)

main =
    T.runTestSuite 5 $ T.testSuite
        [ T.group (T.TestGroupProps "" 5 0 26)
                [ $(T.testcase [e| fib 0 `T.assertEqual` 1 |])
                , $(T.testcase [e| fib 1 `T.assertEqual` 1 |])
                , $(T.testcase [e| fib 2 `T.assertEqual` 2 |])
                , $(T.testcase [e| fib 3 `T.assertEqual` 3 |])
                , $(T.testcase [e| fib 4 `T.assertEqual` 5 |])
                ]
        ]