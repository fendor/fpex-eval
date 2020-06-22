{-# LANGUAGE TemplateHaskell #-}

import Assignment7 hiding (main)
import Data.List as L
import qualified TestSpec as T

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Logic Program Stream" 5 0 20)
          [ $(T.testcase [e|show (run (shuffleLP (var "x", var "y", list [1 .. 4])) :: Stream Answer) `T.assertEqual` "[{x=Nil,y=[1,2,3,4]},{x=[1,2,3,4],y=Nil},{x=[1],y=[2,3,4]},{x=[1,3,4],y=[2]},{x=[1,3],y=[2,4]}]"|]),
            $(T.testcase [e|show (run (append (var "x", list [2, 3], list [1, 2, 3])) :: Stream Answer) `T.assertEqual` "[{x=[1]}]"|]),
            $(T.testcase [e|show (run (good (list [1, 0, 1, 1, 0, 0, 1, 0, 0])) :: Stream Answer) `T.assertEqual` "[{}]"|]),
            $(T.testcase [e|show (run (proper (list [3, 0, 2, 1, 4, 2, 3, 1, 4])) :: Stream Answer) `T.assertEqual` "[{}]"|])
          ],
        T.group
          (T.TestGroupProps "Logic Program Diag" 5 0 20)
          [ $(T.testcase [e|show (run (shuffleLP (var "x", var "y", list [1 .. 4])) :: Diag Answer) `T.assertEqual` "[{x=Nil,y=[1,2,3,4]},{x=[1,2,3,4],y=Nil},{x=[1],y=[2,3,4]},{x=[1,3,4],y=[2]},{x=[1,3],y=[2,4]}]"|]),
            $(T.testcase [e|show (run (append (var "x", list [2, 3], list [1, 2, 3])) :: Diag Answer) `T.assertEqual` "[{x=[1]}]"|]),
            $(T.testcase [e|show (run (good (list [1, 0, 1, 1, 0, 0, 1, 0, 0])) :: Diag Answer) `T.assertEqual` "[{}]"|]),
            $(T.testcase [e|show (run (proper (list [3, 0, 2, 1, 4, 2, 3, 1, 4])) :: Diag Answer) `T.assertEqual` "[{}]"|])
          ],
        T.group
          (T.TestGroupProps "Logic Program Matrix" 5 0 20)
          [ $(T.testcase [e|show (run (shuffleLP (var "x", var "y", list [1 .. 4])) :: Matrix Answer) `T.assertEqual` "MkMatrix [[],[{x=Nil,y=[1,2,3,4]}],[{x=[1,2,3,4],y=Nil}],[{x=[1],y=[2,3,4]}],[{x=[1,3,4],y=[2]}],[{x=[1,3],y=[2,4]}]]"|]),
            $(T.testcase [e|show (run (append (var "x", list [2, 3], list [1, 2, 3])) :: Matrix Answer) `T.assertEqual` "MkMatrix [[],[],[{x=[1]}],[],[]]"|]),
            $(T.testcase [e|show (run (good (list [1, 0, 1, 1, 0, 0, 1, 0, 0])) :: Matrix Answer) `T.assertEqual` "MkMatrix [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[{}]]"|]),
            $(T.testcase [e|show (run (proper (list [3, 0, 2, 1, 4, 2, 3, 1, 4])) :: Matrix Answer) `T.assertEqual` "MkMatrix [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[{}],[],[]]"|])
          ],
        T.group
          (T.TestGroupProps "Interpreter" 5 0 20)
          [ $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 4
                         _ -> error $ "Unknown variable: " ++ x
                       pg =
                         P
                           "P"
                           [ If
                               (Equal (V "x") (I 4))
                               [Ass (V "v") (I 3)]
                               [Ass (V "v") (I 4)]
                           ]
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "v"] (interpreter pg state)
                         `T.assertEqual` [("v", Left 3), ("x", Left 4)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 5
                         _ -> error $ "Unknown variable: " ++ x
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "v"] (interpreter (P "P" [Ass (V "v") (Plu (I 2) (V "x"))]) state)
                         `T.assertEqual` [("v", Left 7), ("x", Left 5)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 17
                         "y" -> Left 56
                         _ -> error $ "Unknown variable: " ++ x
                       pg =
                         P
                           "Gcd"
                           [ While
                               (NEqual (V "x") (V "y"))
                               [ If
                                   (LEqual (V "y") (V "x"))
                                   [Ass (V "x") (Min (V "x") (V "y"))]
                                   [Ass (V "y") (Min (V "y") (V "x"))]
                               ]
                           ]
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "y"] (interpreter pg state)
                         `T.assertEqual` [("x", Left 1), ("y", Left 1)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 10
                         _ -> error $ "Unknown variable: " ++ x
                       pg =
                         P
                           "Factorial"
                           [ Ass (V "result") (I 1),
                             Ass (V "i") (I 1),
                             Repeat [Ass (V "result") (Mul (V "result") (V "i")), Ass (V "i") (Plu (V "i") (I 1))] (Equal (Min (V "i") (I 1)) (V "x"))
                           ]
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "result", "i"] (interpreter pg state)
                         `T.assertEqual` [("i", Left 11), ("result", Left 3628800), ("x", Left 10)]
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Mini Program evaluation" 5 0 20)
          [ $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 4
                         _ -> error $ "Unknown variable: " ++ x
                       pg = "PROGRAM P x = 4; IF == x 4 THEN v = 3 ELSE v = 4 ."
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "v"] (mini_interpreter pg state)
                         `T.assertEqual` [("v", Left 3), ("x", Left 4)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 5
                         _ -> error $ "Unknown variable: " ++ x
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "v"] (mini_interpreter "PROGRAM P v = + 2 x ." state)
                         `T.assertEqual` [("v", Left 7), ("x", Left 5)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 17
                         "y" -> Left 56
                         _ -> error $ "Unknown variable: " ++ x
                       pg = "PROGRAM Gcd BEGIN x = 17; y = 56; WHILE /= x y DO IF <= y x THEN x = - x y ELSE y = - y x END ."
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "y"] (mini_interpreter pg state)
                         `T.assertEqual` [("x", Left 1), ("y", Left 1)]
                   |]
             ),
            $( T.testcase
                 [e|
                   let state x = case x of
                         "x" -> Left 10
                         _ -> error $ "Unknown variable: " ++ x
                       pg = "PROGRAM Factorial result = 1; i = 1; REPEAT BEGIN result = * result i; i = + i 1 END UNTIL == - i 1 x ."
                       listify xs s = L.sortOn fst $ map (\x -> (x, s x)) xs
                    in listify ["x", "result", "i"] (mini_interpreter pg state)
                         `T.assertEqual` [("i", Left 11), ("result", Left 3628800), ("x", Left 10)]
                   |]
             )
          ]
      ]

