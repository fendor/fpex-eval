{-# LANGUAGE TemplateHaskell #-}
module TestSuite1 where
import qualified TestSpec as T
import           Assignment0 hiding (main)

main =
    T.runTestSuite 5 $ T.testSuite
        [ T.group (T.TestGroupProps "Factorial" 2 0 10)
            [ $(T.testcase [e| factorial 0 `T.assertEqual` 1 |])
            , $(T.testcase [e| factorial 1 `T.assertEqual` 1 |])
            , $(T.testcase [e| factorial 2 `T.assertEqual` 2 |])
            , $(T.testcase [e| factorial 3 `T.assertEqual` 6 |])
            , $(T.testcase [e| factorial 15 `T.assertEqual` 1307674368000 |])
            , $(T.testcase [e| factorial 20 `T.assertEqual` 2432902008176640000 |])
            ]
        ]