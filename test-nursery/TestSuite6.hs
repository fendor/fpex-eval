{-# LANGUAGE TemplateHaskell #-}

import Assignment6 hiding (main)
import qualified TestSpec as T

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Applicative Parser Basic Statements" 3 0 15)
          [ $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = 2 ."
                     `T.assertEqual` P "P" [Ass (V "v") (I 2)]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = + 2 2 ."
                     `T.assertEqual` P "P" [Ass (V "v") (Plu (I 2) (I 2))]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = 2; x = / v v ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (I 2),
                         Ass (V "x") (Div (V "v") (V "v"))
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = 2.2 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (F 2.2)
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = * + 3 2 4 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (Mul (Plu (I 3) (I 2)) (I 4))
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P IF <= + x 1 y THEN x = 3 ELSE x = 2 ."
                     `T.assertEqual` P
                       "P"
                       [ If
                           (LEqual (Plu (V "x") (I 1)) (V "y"))
                           [Ass (V "x") (I 3)]
                           [Ass (V "x") (I 2)]
                       ]
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Applicative Parser Complex Statements" 5 0 25)
          [ $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P IF <= 3 4 THEN v = 3 ELSE v = 4 ."
                     `T.assertEqual` P
                       "P"
                       [ If
                           (LEqual (I 3) (I 4))
                           [Ass (V "v") (I 3)]
                           [Ass (V "v") (I 4)]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P v = 2; IF <= 3 4 THEN v = 3 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (I 2),
                         If
                           (LEqual (I 3) (I 4))
                           [Ass (V "v") (I 3)]
                           []
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P x = 4; IF == x 4 THEN v = 3 ELSE v = 4 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         If
                           (Equal (V "x") (I 4))
                           [Ass (V "v") (I 3)]
                           [Ass (V "v") (I 4)]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P x = 4; WHILE >= x 0 DO x = - x 1 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         While
                           (GEqual (V "x") (I 0))
                           [Ass (V "x") (Min (V "x") (I 1))]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM P x = 4; REPEAT x = - x 1 UNTIL >= x 0 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         Repeat
                           [Ass (V "x") (Min (V "x") (I 1))]
                           (GEqual (V "x") (I 0))
                       ]
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Applicative Parser Real Programs" 5 0 10)
          [ $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM Gcd BEGIN x = 17; y = 56; WHILE /= x y DO IF <= y x THEN x = - x y ELSE y = - y x END ."
                     `T.assertEqual` P
                       "Gcd"
                       [ Ass (V "x") (I 17),
                         Ass (V "y") (I 56),
                         While
                           (NEqual (V "x") (V "y"))
                           [ If
                               (LEqual (V "y") (V "x"))
                               [Ass (V "x") (Min (V "x") (V "y"))]
                               [Ass (V "y") (Min (V "y") (V "x"))]
                           ]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel1 parser1 "PROGRAM Factorial x = 10; result = 1; i = 1; REPEAT result = * result i UNTIL <= i x ."
                     `T.assertEqual` P
                       "Factorial"
                       [ Ass (V "x") (I 10),
                         Ass (V "result") (I 1),
                         Ass (V "i") (I 1),
                         Repeat [Ass (V "result") (Mul (V "result") (V "i"))] (LEqual (V "i") (V "x"))
                       ]
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Monad Parser Basic Statements" 3 0 15)
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
          ],
        T.group
          (T.TestGroupProps "Monad Parser Complex Statements" 5 0 25)
          [ $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P IF <= 3 4 THEN v = 3 ELSE v = 4 ."
                     `T.assertEqual` P
                       "P"
                       [ If
                           (LEqual (I 3) (I 4))
                           [Ass (V "v") (I 3)]
                           [Ass (V "v") (I 4)]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P v = 2; IF <= 3 4 THEN v = 3 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "v") (I 2),
                         If
                           (LEqual (I 3) (I 4))
                           [Ass (V "v") (I 3)]
                           []
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P x = 4; IF == x 4 THEN v = 3 ELSE v = 4 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         If
                           (Equal (V "x") (I 4))
                           [Ass (V "v") (I 3)]
                           [Ass (V "v") (I 4)]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P x = 4; WHILE >= x 0 DO x = - x 1 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         While
                           (GEqual (V "x") (I 0))
                           [Ass (V "x") (Min (V "x") (I 1))]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM P x = 4; REPEAT x = - x 1 UNTIL >= x 0 ."
                     `T.assertEqual` P
                       "P"
                       [ Ass (V "x") (I 4),
                         Repeat
                           [Ass (V "x") (Min (V "x") (I 1))]
                           (GEqual (V "x") (I 0))
                       ]
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Monad Parser Real Programs" 5 0 10)
          [ $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM Gcd BEGIN x = 17; y = 56; WHILE /= x y DO IF <= y x THEN x = - x y ELSE y = - y x END ."
                     `T.assertEqual` P
                       "Gcd"
                       [ Ass (V "x") (I 17),
                         Ass (V "y") (I 56),
                         While
                           (NEqual (V "x") (V "y"))
                           [ If
                               (LEqual (V "y") (V "x"))
                               [Ass (V "x") (Min (V "x") (V "y"))]
                               [Ass (V "y") (Min (V "y") (V "x"))]
                           ]
                       ]
                   |]
             ),
            $( T.testcase
                 [e|
                   topLevel2 parser2 "PROGRAM Factorial x = 10; result = 1; i = 1; REPEAT result = * result i UNTIL <= i x ."
                     `T.assertEqual` P
                       "Factorial"
                       [ Ass (V "x") (I 10),
                         Ass (V "result") (I 1),
                         Ass (V "i") (I 1),
                         Repeat [Ass (V "result") (Mul (V "result") (V "i"))] (LEqual (V "i") (V "x"))
                       ]
                   |]
             )
          ]
      ]