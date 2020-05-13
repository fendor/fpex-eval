{-# LANGUAGE TemplateHaskell #-}

import Assignment4 hiding (main)
import qualified TestSpec as T
import qualified Data.Ratio as R

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Greedy Search" 5 0 15)
          [ $(T.testcase [|greedy (2,4) `T.assertEqual` [2]|])
          , $(T.testcase [|greedy (9,20) `T.assertEqual` [3,9,180]|])
          , $(T.testcase [|greedy (5,31) `T.assertEqual` [7,55,3979,23744683,1127619917796295]|])
          ]
      , T.group
          (T.TestGroupProps "Naive Generator" 5 0 15)
          [ $(T.testcase [|
              let
                (n, d) = (1,2)
                query = gen (n ,d) 10
                allValid = all (== (n R.% d))
                    (map (sum . map (1 R.%)) query)
              in allValid && (length query == 2)
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,6)
                query = gen (n, d) 100
                allValid = all (== (n R.% d))
                    (map (sum . map (1 R.%)) $ take 2 query)
              in allValid && (not $ null query)
                   `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,31)
                query = gen (n, d) 3721
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
              in allValid && (not $ null query)
                  `T.assertEqual` True|])
          ]
      , T.group
          (T.TestGroupProps "Smallest number of summands" 5 0 15)
          [ $(T.testcase [|
              let
                (n, d) = (1,2)
                query = gs1 (n, d) 10
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) query)
              in allValid && all ((== 1) . length) query && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,6)
                query = gs1 (n, d) 20
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
              in allValid && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,31)
                query = gs1 (n, d) 250
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
              in allValid && all ((== 3) . length) query
                  `T.assertEqual` True|])
          ]
      , T.group
          (T.TestGroupProps "Largest denominator" 5 0 15)
          [ $(T.testcase [|gs2 (1, 2) 10 `T.assertEqual` Just [2]|])
          , $(T.testcase [|gs2 (3, 4) 5 `T.assertEqual` Just [2,4]|])
          , $(T.testcase [|gs2 (5, 31) 42 `T.assertEqual` Nothing|])
          ]
      , T.group
          (T.TestGroupProps "Backtracking search (Smallest number of summands)" 5 0 20)
          [ $(T.testcase [|
              let
                (n, d) = (1,2)
                query = bt1 (n, d) 10 2
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) query)
              in allValid && all ((== 1) . length) query && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,6)
                query = bt1 (n, d) 100 250
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
              in allValid && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|null (bt1 (5, 31) 40 30) `T.assertEqual` True|])
          , $(T.testcase [|bt1 (4, 5) 20 8 `T.assertEqual` [[3,5,6,10]]|])
          ]
      , T.group
          (T.TestGroupProps "Backtracking search (Largest Denominator)" 5 0 20)
          [ $(T.testcase [|
              let
                (n, d) = (1,2)
                query = bt2 (n, d) 10 5
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) query)
              in allValid && all ((<= 2) . length) query && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5,6)
                query = bt2 (n, d) 100 4
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
                allSmaller = all ((<= 4) . length) $ take 4 query
              in allValid && (not $ null query) && allSmaller
                  `T.assertEqual` True|])
          , $(T.testcase [|
              let
                (n, d) = (5, 31)
                query = bt2 (n, d) 3721 3
                allValid = all (== (n R.% d))
                  (map (sum . map (1 R.%)) $ take 2 query)
                allSmaller = all ((<= 3) . length) $ take 2 query
              in allValid && allSmaller && (not $ null query)
                  `T.assertEqual` True|])
          , $(T.testcase [|bt2 (5, 31) 60 2 `T.assertEqual` []|])
          ]
      ]



