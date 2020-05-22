{-# LANGUAGE TemplateHaskell #-}

import Assignment5 hiding (main)
import qualified TestSpec as T
import qualified Test.QuickCheck as Q

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group (T.TestGroupProps "All Properties" 5 0 25)
        [ $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_1) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_2) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_3) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_4) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_5) `T.assertEqualIO` True |])
        ]
      , T.group (T.TestGroupProps "Classifiers" 5 0 25)
        [ $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_3_collect) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_3_trivial) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_ccc_3_classify) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult (prop_ccd 10)) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_greedy) `T.assertEqualIO` True |])
        ]
        , T.group (T.TestGroupProps "Stack property" 5 0 20)
        [ $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_31) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_32) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_34) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_36) `T.assertEqualIO` True |])
        ]
        , T.group (T.TestGroupProps "Tree numbering" 10 0 30)
        [ $(T.testcase [e| number1 (Node 12 (Node 4 Leaf Leaf) (Node 4 Leaf Leaf)) `T.assertEqual` Node 0 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf) |])
        , $(T.testcase [e| number  (Node 12 (Node 4 Leaf Leaf) (Node 4 Leaf Leaf)) `T.assertEqual` Node 0 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf) |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckResult prop_rename) `T.assertEqualIO` True |])
        ]
      ]



