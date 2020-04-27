{-# LANGUAGE TemplateHaskell #-}

import qualified TestSpec as T
import           Assignment2 hiding (main)
import qualified Data.Array as A
import qualified Data.Maybe as M

main :: IO ()
main =
    T.runTestSuite 5 $ T.testSuite
        [ T.group (T.TestGroupProps "Stirling numbers" 5 0 20)
            [ $(T.testcase
                [e| (take 4 sta) `T.assertEqual`
                    [ A.array (0,0) [(0, Just 1)]
                    , A.array (-1,1) [(-1, Just 1), (0, Nothing), (1, Just 1)]
                    , A.array (-2,2) [(-2, Just 1), (-1, Nothing), (0, Just 3), (1, Nothing), (2, Just 1)]
                    , A.array (-3,3) [(-3, Just 1), (-2, Nothing), (-1, Just 7), (0, Nothing), (1, Just 6), (2, Nothing), (3, Just 1)]
                    ]
                |])
            , $(T.testcase [e| (sta !! 6) A.! 0 `T.assertEqual` Just 350|])
            , $(T.testcase [e| (sta !! 10) A.! (-10) `T.assertEqual` Just 1|])
            , $(T.testcase [e| sum (fmap (M.fromMaybe 0) (sta !! 7)) `T.assertEqual` 4140|])
            ]
        , T.group (T.TestGroupProps "Conversion" 5 0 15)
            [ $(T.testcase [e| conv (sta !! 0) `T.assertEqual` [1]|])
            , $(T.testcase [e| conv (sta !! 3) `T.assertEqual` [1, 7, 6, 1]|])
            , $(T.testcase [e| conv (sta !! 6) `T.assertEqual` [1, 63, 301, 350, 140, 21, 1]|])
            ]
        , T.group (T.TestGroupProps "Pretty print stirling numbers" 5 0 15)
            [ $(T.testcase [e| pretty_print [] `T.assertEqual` []|])
            , $(T.testcase [e| pretty_print (take 4 sta) `T.assertEqual` [[1], [1,1], [1,3,1], [1,7,6,1]]|])
            -- sufficiently infinite
            , $(T.testcase [e| length (take 1000 (pretty_print sta)) `T.assertEqual` 1000 |])
            ]
        , T.group (T.TestGroupProps "Knight Numbers" 5 0 20)
            [ $(T.testcase [e| take 5 kns `T.assertEqual` [1, 1, 2, 4, 9]|])
            , $(T.testcase [e| kns !! 7 `T.assertEqual` 164|])
            -- sufficiently infinite
            , $(T.testcase [e| length (take 1000 kns) `T.assertEqual` 1000|])
            , $(T.testcase [e| length [n | n <- takeWhile (<200) kns, mod n 2 == 0] `T.assertEqual` 5 |])
            ]
        , T.group (T.TestGroupProps "Modified Post's Correspondence Problem(MPCP)" 5 0 30)
            [ $(T.testcase [e|mcpc_eq (["00","11","010"],["11","00","010"]) 0 `T.assertEqual` Nothing |])
            , $(T.testcase [e|mcpc_eq (["00","11","010"],["0","1011","00"]) 2 `T.assertEqual` Just [2,1] |])
            , $(T.testcase [e|mcpc_eq (["0","1","11","111","11111","01"],["00","11111","11","11","1","11"]) 5 `T.assertEqual` Just [5,1,2,3,4] |])

            , $(T.testcase [e|mcpc_le (["00","1","1"],["0","01","01"]) 2 `T.assertEqual` Just [1] |])
            , $(T.testcase [e|mcpc_le (["00","1","1","11"],["0","0","11","011"]) 2 `T.assertEqual` Just [3] |])
            , $(T.testcase [e|mcpc_le (["111111", "0", "0", "0", "0", "0", "0"], ["1", "1110", "11", "00", "1", "000", "111"]) 4 `T.assertEqual` Just [2,1,3] |])
            ]
        ]