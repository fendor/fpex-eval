{-# LANGUAGE TemplateHaskell #-}

import Assignment7 hiding (main)
import qualified TestSpec as T

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Logic Prigram Stream" 4 0 20)
          [],
        T.group
          (T.TestGroupProps "Logic Prigram Diag" 4 0 20)
          [],
        T.group
          (T.TestGroupProps "Logic Prigram Stream" 4 0 20)
          [],
        T.group
          (T.TestGroupProps "Interpreter" 4 0 20)
          [ $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = 2 ."
                     `T.assertEqual` P "P" [Ass (V "v") (I 2)]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = + 2 2 ."
                     `T.assertEqual` P "P" [Ass (V "v") (Plu (I 2) (I 2))]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = 2; x = / v v ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (I 2),
                         Ass (V "x") (Div (V "v") (V "v"))
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = 2.2 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (F 2.2)
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = * + 3 2 4 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (Mul (Plu (I 3) (I 2)) (I 4))
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P IF <= + x 1 y THEN x = 3 ELSE x = 2 ."
                     `T.assertEqual` P
                       "P"
                       [ If
                           (LEqual (Plu (V "x") (I 1)) (V "y"))
                           [Ass (V "x") (I 3)]
                           [Ass (V "x") (I 2)]
                       ]
                   |]
             )
          ]
          , T.group
          (T.TestGroupProps "Mini Program evaluation" 4 0 20) []
      ]
