{-# LANGUAGE TemplateHaskell #-}

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
        , T.group (T.TestGroupProps "Fibbonacci" 2 0 10)
            [ $(T.testcase [e| fibonacci 0 `T.assertEqual`   0  |])
            , $(T.testcase [e| fibonacci 1 `T.assertEqual`   1 |])
            , $(T.testcase [e| fibonacci 10 `T.assertEqual` 55 |])
            , $(T.testcase [e| fibonacci 50 `T.assertEqual` 12586269025  |])
            , $(T.testcase [e| length (show $ fibonacci 600) `T.assertEqual` 126 |])
            ]
        , T.group (T.TestGroupProps "Binom" 2 0 10)
            [ $(T.testcase [e| binom 20  0 `T.assertEqual` 1  |])
            , $(T.testcase [e| binom 20  1 `T.assertEqual` 20 |])
            , $(T.testcase [e| binom 20 20 `T.assertEqual` 1  |])
            , $(T.testcase [e| binom 20 19 `T.assertEqual` 20  |])
            , $(T.testcase [e| binom 20 10 `T.assertEqual` 184756 |])
            ]
        , T.group (T.TestGroupProps "Faulty Factorial" 2 0 10)
            [ $(T.testcase [e| fac_faulty 0 `T.assertEqual` 1 |])
            , $(T.testcase [e| fac_faulty 1 `T.assertEqual` 1 |])
            , $(T.testcase [e| fac_faulty 2 `T.assertEqual` 2 |])
            , $(T.testcase [e| fac_faulty 3 `T.assertEqual` 6 |])
            , $(T.testcase [e| fac_faulty 15 `T.assertEqual` 1307674368000 |])
            , $(T.testcase [e| fac_faulty 20 `T.assertEqual` 2432902008176640000 |])
            ]
        , T.group (T.TestGroupProps "Faulty Fibbonacci" 2 0 10)
            [ $(T.testcase [e| fib_faulty 0 `T.assertEqual` 0   |])
            , $(T.testcase [e| fib_faulty 1 `T.assertEqual` 1   |])
            , $(T.testcase [e| fib_faulty 10 `T.assertEqual` 55 |])
            , $(T.testcase [e| fib_faulty 50 `T.assertEqual` 12586269025  |])
            , $(T.testcase [e| length (show $ fib_faulty 600) `T.assertEqual` 126 |])
            ]
        , T.group (T.TestGroupProps "Faulty Binom" 2 0 10)
            [ $(T.testcase [e| binom_faulty 20  0 `T.assertEqual` 1  |])
            , $(T.testcase [e| binom_faulty 20  1 `T.assertEqual` 20 |])
            , $(T.testcase [e| binom_faulty 20 20 `T.assertEqual` 1  |])
            , $(T.testcase [e| binom_faulty 20 19 `T.assertEqual` 20 |])
            , $(T.testcase [e| binom_faulty 20 10 `T.assertEqual` 184756 |])
            ]
        , T.group (T.TestGroupProps "Slow Fibbonacci" 10 0 10)
            [ $(T.testcase [e| length (show $ fib_slow 600) `T.assertEqual` 126 |])
            ]
        , T.group (T.TestGroupProps "Not implemented \"foo\"" 2 0 10)
            [ $(T.testcase [e| foo "test1" `T.assertEqual` "Ok" |])
            , $(T.testcase [e| foo "test2" `T.assertEqual` "Ok" |])
            , $(T.testcase [e| foo "test3" `T.assertEqual` "Ok" |])
            , $(T.testcase [e| foo "test4" `T.assertEqual` "Ok" |])
            , $(T.testcase [e| foo "test5" `T.assertEqual` "Ok" |])
            ]
        ]