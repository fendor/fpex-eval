{-# LANGUAGE TemplateHaskell #-}

import Assignment5 hiding (main)
import qualified TestSpec as T
import qualified Test.QuickCheck as Q

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group (T.TestGroupProps "All Properties" 5 0 25)
        [ $(T.testcase [e| fmap (not . Q.isSuccess) (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_1) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_2) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_3) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap (not . Q.isSuccess) (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_4) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_5) `T.assertEqualIO` True |])
        ]
      , T.group (T.TestGroupProps "Classifiers" 5 0 25)
        [ $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_3_collect) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_3_trivial) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccc_3_classify) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap (not . Q.isSuccess) (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_ccd) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap (not . Q.isSuccess) (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_greedy) `T.assertEqualIO` True |])
        ]
        , T.group (T.TestGroupProps "Stack property" 5 0 20)
        [ $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs {Q.chatty = False} prop_31) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs {Q.chatty = False} prop_32) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs {Q.chatty = False} prop_34) `T.assertEqualIO` True |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs {Q.chatty = False} prop_36) `T.assertEqualIO` True |])
        ]
        , T.group (T.TestGroupProps "Tree numbering" 10 0 30)
        [ $(T.testcase [e| show (number1 (Node (12 :: Integer) (Node 4 Nil Nil) (Node 4 Nil Nil))) `T.assertEqual` "Node 0 (Node 1 Nil Nil) (Node 1 Nil Nil)" |])
        , $(T.testcase [e| show (number  (Node (12 :: Integer) (Node 4 Nil Nil) (Node 4 Nil Nil))) `T.assertEqual` "Node 0 (Node 1 Nil Nil) (Node 1 Nil Nil)" |])
        , $(T.testcase [e| fmap Q.isSuccess (Q.quickCheckWithResult Q.stdArgs { Q.chatty = False} prop_rename) `T.assertEqualIO` True |])
        ]
      ]



