{-# LANGUAGE TemplateHaskell #-}

import qualified TestSpec as T
import           Assignment1 hiding (main)

main =
    T.runTestSuite 5 $ T.testSuite
        [ T.group (T.TestGroupProps "Stirling numbers" 5 0 20)
            [ $(T.testcase [e| (take 5 st) `T.assertEqual` [[1],[1,1],[1,3,1],[1,7,6,1],[1,15,25,10,1]] |])
            , $(T.testcase [e| (head (drop 3 st)) `T.assertEqual` [1,7,6,1] |])
            , $(T.testcase [e| take 5 [rs | rs <- st, mod (length rs) 2 /= 0] `T.assertEqual`
                               [ [1], [1,3,1], [1,15,25,10,1]
                               , [1,63,301,350,140,21,1]
                               , [1,255,3025,7770,6951,2646,462,36,1]
                               ]
                           |]
               )
            -- Sufficiently infinite
            , $(T.testcase [e| length (take 10000 st) `T.assertEqual` 10000|])
            ]
        , T.group (T.TestGroupProps "Bell numbers" 5 0 20)
            [ $(T.testcase [e| (take 5 bn) `T.assertEqual` [1,2,5,15,52] |])
            , $(T.testcase [e| (head (drop 3 bn)) `T.assertEqual` 15 |])
            , $(T.testcase [e| (take 5 [n | n <- bn, mod n 2 /= 0]) `T.assertEqual` [1,5,15,203,877] |])
            -- Sufficiently infinite
            , $(T.testcase [e| length (take 10000 bn) `T.assertEqual` 10000|])
            ]
        , T.group (T.TestGroupProps "Test 'f'" 5 0 10)
            [ $(T.testcase [e| (abs ((f 10 10) - 11008.615520898074) < 0.001) `T.assertEqual` True |])
            , $(T.testcase [e| (abs ((f 1 1)   - 1.1666666666666667) < 0.001) `T.assertEqual` True |])
            ]
        , T.group (T.TestGroupProps "Test 'f_mt'" 5 0 10)
            [ $(T.testcase [e| (abs ((f_mt 10 10) - 11008.615520898074) < 0.001) `T.assertEqual` True |])
            , $(T.testcase [e| (abs ((f_mt 1 1) -   1.1666666666666667) < 0.001) `T.assertEqual` True |])
            ]
        , T.group (T.TestGroupProps "Binom wih stream programming" 2 0 10)
            [ $(T.testcase [e| binom_stpg (10, 10) `T.assertEqual` 1 |])
            , $(T.testcase [e| binom_stpg (10, 9) `T.assertEqual` 10 |])
            , $(T.testcase [e| binom_stpg (10, 5) `T.assertEqual` 252 |])
            , $(T.testcase [e| binom_stpg (10, 1) `T.assertEqual` 10 |])
            , $(T.testcase [e| binom_stpg (10, 0) `T.assertEqual` 1 |])
            ]
        , T.group (T.TestGroupProps "Binom wih stream programming (big numbers)" 10 0 10)
            [ $(T.testcase [e| binom_stpg (70, 60) `T.assertEqual` 396704524216 |])
            ]
        , T.group (T.TestGroupProps "Binom wih memoization" 2 0 10)
            [ $(T.testcase [e| binom_memo (10, 10) `T.assertEqual` 1 |])
            , $(T.testcase [e| binom_memo (10, 9)  `T.assertEqual` 10 |])
            , $(T.testcase [e| binom_memo (10, 5) `T.assertEqual` 252 |])
            , $(T.testcase [e| binom_memo (10, 1) `T.assertEqual` 10 |])
            , $(T.testcase [e| binom_memo (10, 0) `T.assertEqual` 1 |])
            ]
        , T.group (T.TestGroupProps "Binom wih memoization (big numbers)" 10 0 10)
            [ $(T.testcase [e| binom_memo (70, 60) `T.assertEqual` 396704524216 |])
            ]
        ]