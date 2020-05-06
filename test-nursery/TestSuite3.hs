{-# LANGUAGE TemplateHaskell #-}

import Assignment3 hiding (main)
import qualified Data.Array as A
import qualified TestSpec as T
import Prelude hiding (Ordering (..))

main :: IO ()
main =
  T.runTestSuite 5 $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Naive solvers" 5 0 10)
          [ $( T.testcase
                 [e|
                   let p = LP [ [C, S, B, S],[S, C, S, C],[C, B, C, S],[S, C, S, B]]
                         $ \box -> case box of 1 -> [(One, One), (Two, One)] ;2 -> [(One, Two), (One, Three)] ;3 -> [(One, Four), (Two, Four)] ;4 -> [(Two, Two), (Two, Three)] ;5 -> [(Three, One), (Four, One)] ;6 -> [(Three, Two), (Four, Two)] ;7 -> [(Three, Three), (Three, Four)] ;8 -> [(Four, Three), (Four, Four)]
                       p_s = [[C, S, C, S], [S, C, S, C], [C, S, C, S], [S, C, S, C]]
                    in p_solve_init p `T.assertEqual` Just p_s
                   |]
             ),
            $( T.testcase
                 [e|
                   let t = LT [ [B, T, T, C, S, B],[S, T, C, S, T, C],[T, C, T, C, S, C],[C, T, S, T, S, T],[C, S, T, C, S, C],[B, T, T, S, T, B]]
                           $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                       t_s = [ [C, T, T, C, S, C],[S, T, C, S, T, C],[T, C, T, C, S, C],[C, T, S, T, S, T],[C, S, T, C, S, C],[C, T, T, S, T, S]]
                    in t_solve_init t `T.assertEqual` Just t_s
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Smart solvers" 15 0 30)
          [ $( T.testcase
                 [e|
                   let p2 = LP [ [B, C, B, B],[B, B, B, S],[B, B, B, B],[C, B, B, B] ]
                         $ \box -> case box of 1 -> [(One, One), (One, Two)];2 -> [(One, Three), (One, Four)];3 -> [(Two, One), (Three, One)];4 -> [(Two, Two), (Two, Three)];5 -> [(Two, Four), (Three, Four)];6 -> [(Three, Two), (Three, Three)];7 -> [(Four, One), (Four, Two)];8 -> [(Four, Three), (Four, Four)]
                       p2_solved = [ [S, C, S, C],[C, S, C, S],[S, C, S, C],[C, S, C, S]]
                    in p_solve p2 `T.assertEqual` Just p2_solved
                   |]
             ),
            $( T.testcase
                 [e|
                   let t2 = LT [ [B, B, C, B, S, B],[B, C, B, B, B, B],[T, B, B, B, C, B],[B, T, B, B, B, C],[B, B, B, B, T, B],[B, T, B, C, B, B]]
                         $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)]; 2 -> [(One, Two), (Two, Two), (Three, Two)]; 3 -> [(One, Three), (One, Four), (Two, Three)]; 4 -> [(One, Five), (One, Six), (Two, Six)]; 5 -> [(Two, Four), (Two, Five), (Three, Five)]; 6 -> [(Three, Three), (Three, Four), (Four, Four)]; 7 -> [(Three, Six), (Four, Five), (Four, Six)]; 8 -> [(Four, One), (Four, Two), (Five, Two)]; 9 -> [(Four, Three), (Five, Three), (Six, Three)]; 10 -> [(Five, One), (Six, One), (Six, Two)]; 11 -> [(Five, Four), (Five, Five), (Five, Six)]; 12 -> [(Six, Four), (Six, Five), (Six, Six)]
                       t2_solved = [ [C, T, C, T, S, S],[S, C, S, C, C, S],[T, S, T, S, C, T],[S, T, S, C, S, C],[T, C, S, T, T, T],[T, T, S, C, C, C]]
                    in t_solve t2 `T.assertEqual` Just t2_solved
                   |]
             )
          ],
        T.group
          (T.TestGroupProps "Naive solvers (array representation)" 5 0 10)
          [ $( T.testcase
                 [e|
                   let ap_trivial = AP (A.listArray ((One, One), (Four, Four)) [ C,S,C,B,S,C,S,B,C,S,C,B,S,C,S,B])
                         $ \box -> case box of 1 -> [(One, One), (Two, One)]; 2 -> [(One, Two), (One, Three)]; 3 -> [(One, Four), (Two, Four)]; 4 -> [(Two, Two), (Two, Three)]; 5 -> [(Three, One), (Four, One)]; 6 -> [(Three, Two), (Four, Two)]; 7 -> [(Three, Three), (Three, Four)]; 8 -> [(Four, Three), (Four, Four)]
                       ap_trivial_solved = A.listArray ((One, One), (Four, Four)) [C, S, C, S, S, C, S, C, C, S, C, S, S, C, S, C]
                    in ap_solve_init ap_trivial `T.assertEqual` Just ap_trivial_solved
                   |]
             ),
            $(T.testcase [e|
              let
                at_trivial = AT ( A.listArray ((One, One), (Six, Six)) [C,T,T,C,S,C,S,T,C,S,T,C,B,C,T,C,S,C,B,T,S,T,S,T,B,S,T,C,S,C,B,T,T,S,T,S] )
                  $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)]; 2 -> [(One, Two), (One, Three), (Two, Two)]; 3 -> [(One, Four), (One, Five), (Two, Five)]; 4 -> [(One, Six), (Two, Six), (Three, Six)]; 5 -> [(Two, Three), (Three, Three), (Four, Three)]; 6 -> [(Two, Four), (Three, Four), (Four, Four)]; 7 -> [(Three, Two), (Four, Two), (Five, Two)]; 8 -> [(Three, Five), (Four, Five), (Five, Five)]; 9 -> [(Four, One), (Five, One), (Six, One)]; 10 -> [(Four, Six), (Five, Six), (Six, Six)]; 11 -> [(Five, Three), (Six, Two), (Six, Three)]; 12 -> [(Five, Four), (Six, Four), (Six, Five)]
                at_trivial_solved = A.listArray ((One, One), (Six, Six)) [C,T,T,C,S,C,S,T,C,S,T,C,T,C,T,C,S,C,C,T,S,T,S,T,C,S,T,C,S,C,C,T,T,S,T,S]
              in at_solve_init at_trivial `T.assertEqual` Just at_trivial_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart solvers (array representation)" 15 0 30)
          [ $(T.testcase [e|
              let
                ap2 = AP ( A.listArray ((One, One), (Four, Four)) [ B,C,B,B,B,B,B,S,B,B,B,B,C,B,B,B]) ap2_box

                ap2_box = \box -> case box of 1 -> [(One, One), (One, Two)];2 -> [(One, Three), (One, Four)];3 -> [(Two, One), (Three, One)];4 -> [(Two, Two), (Two, Three)];5 -> [(Two, Four), (Three, Four)];6 -> [(Three, Two), (Three, Three)];7 -> [(Four, One), (Four, Two)];8 -> [(Four, Three), (Four, Four)]

                ap2_solved = A.listArray ((One, One), (Four, Four))[ S,C,S,C,C,S,C,S,S,C,S,C,C,S,C,S]
              in ap_solve ap2 `T.assertEqual` Just ap2_solved|]),

            $(T.testcase [e|
              let
                at2 = AT (A.listArray ((One, One), (Six, Six)) [ B,B,C,B,S,B,B,C,B,B,B,B,T,B,B,B,C,B,B,T,B,B,B,C,B,B,B,B,T,B,B,T,B,C,B,B])
                  $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (Two, Two), (Three, Two)];3 -> [(One, Three), (One, Four), (Two, Three)];4 -> [(One, Five), (One, Six), (Two, Six)];5 -> [(Two, Four), (Two, Five), (Three, Five)];6 -> [(Three, Three), (Three, Four), (Four, Four)];7 -> [(Three, Six), (Four, Five), (Four, Six)];8 -> [(Four, One), (Four, Two), (Five, Two)];9 -> [(Four, Three), (Five, Three), (Six, Three)];10 -> [(Five, One), (Six, One), (Six, Two)];11 -> [(Five, Four), (Five, Five), (Five, Six)];12 -> [(Six, Four), (Six, Five), (Six, Six)]

                at2_solved = A.listArray ((One, One), (Six, Six)) [ C,T,C,T,S,S,S,C,S,C,C,S,T,S,T,S,C,T,S,T,S,C,S,C,T,C,S,T,T,T,T,T,S,C,C,C]
              in at_solve at2 `T.assertEqual` Just at2_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart predicates" 1 0 20)
          [ $(T.testcase [e|t_sound (LT [[B]] (const [])) `T.assertEqual` False|]),
            $(T.testcase [e|
                   let t1 = LT [ [B, T, T, C, S, B],[S, T, C, S, T, C],[T, C, T, C, S, C],[C, T, S, T, S, T],[C, S, T, C, S, C],[B, T, T, S, T, B]]
                           $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                    in t_sound t1 `T.assertEqual` True|]),
            $(T.testcase [e|
                   let t1 = LT [ [B, T, T, C, S, B],[S, T, C, S, T, C],[T, C, T, C, S, C],[C, T, S, T, S, T],[C, S, T, C, S, C],[B, T, T, S, T, B]]
                           $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                    in t_complete t1 `T.assertEqual` False|]),
            $(T.testcase [e|
                   let t1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                       t1_solved = [[C, S, C, S], [S, C, S, C], [C, S, C, S], [S, C, S, C]]
                    in t_complete (LT t1_solved t1_box) `T.assertEqual` True|]),
            $(T.testcase [e|
                   let t1 = LT [ [B, T, T, C, S, B],[S, T, C, S, T, C],[T, C, T, C, S, C],[C, T, S, T, S, T],[C, S, T, C, S, C],[B, T, T, S, T, B]]
                           $ \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                    in t_correct t1 `T.assertEqual` False|]),
            $(T.testcase [e|
                   let t1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                       t1_solved = [[C, S, C, S], [S, C, S, C], [C, S, C, S], [S, C, S, C]]
                    in t_correct (LT t1_solved t1_box) `T.assertEqual` True|]),
            $(T.testcase [e|p_sound (LP [[B]] (const [])) `T.assertEqual` False|]),
            $( T.testcase
                 [e|
                   let p1 = LP [ [C, B, B, B],[B, B, S, B],[B, B, B, B],[B, B, B, C]] p1_box
                       p1_box = \box -> case box of; 1 -> [(One, One), (Two, One)]; 2 -> [(One, Two), (One, Three)]; 3 -> [(One, Four), (Two, Four)]; 4 -> [(Two, Two), (Two, Three)]; 5 -> [(Three, One), (Four, One)]; 6 -> [(Three, Two), (Four, Two)]; 7 -> [(Three, Three), (Three, Four)]; 8 -> [(Four, Three), (Four, Four)]
                    in p_sound p1 `T.assertEqual` True
                   |]
             ),
            $( T.testcase
                 [e|
                   let p1 = LP [ [C, B, B, B],[B, B, S, B],[B, B, B, B],[B, B, B, C]] p1_box
                       p1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                    in p_complete p1 `T.assertEqual` False
                   |]
             ),
            $( T.testcase
                 [e|
                   let p1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                       p1_solved =[ [C, S, C, S],[S, C, S, C],[C, S, C, S],[S, C, S, C]]
                    in p_complete (LP p1_solved p1_box) `T.assertEqual` True
                   |]
             ),
            $( T.testcase
                 [e|
                   let p1 = LP [ [C, B, B, B],[B, B, S, B],[B, B, B, B],[B, B, B, C]] p1_box
                       p1_box = \box -> case box of;1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                    in p_correct p1 `T.assertEqual` False
                   |]
             ),
            $( T.testcase
                 [e|
                   let p1_box = \box -> case box of;1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                       p1_solved = [ [C, S, C, S],[S, C, S, C],[C, S, C, S],[S, C, S, C]]
                    in p_correct (LP p1_solved p1_box) `T.assertEqual` True
                   |]
             ),
            $(T.testcase [e|at_sound (AT (A.listArray ((One, One), (One, One)) [B]) (const [])) `T.assertEqual` False|]),
            $(T.testcase [e|
                let
                  at1 = AT( A.listArray ((One, One), (Six, Six)) [ C,T,B,C,B,B,B,B,B,B,B,C,B,B,B,C,B,B,B,B,S,B,B,B,C,B,B,B,B,B,B,B,T,B,T,S]) at1_box
                  at1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                in at_sound at1 `T.assertEqual` True|]),
            $(T.testcase [e|
                let
                  at1 = AT( A.listArray ((One, One), (Six, Six)) [ C,T,B,C,B,B,B,B,B,B,B,C,B,B,B,C,B,B,B,B,S,B,B,B,C,B,B,B,B,B,B,B,T,B,T,S]) at1_box
                  at1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                in at_complete at1 `T.assertEqual` False|]),
            $(T.testcase [e|
                let
                  at1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                  at1_solved = A.listArray ((One, One), (Six, Six)) [ C,T,T,C,S,C,S,T,C,S,T,C,T,C,T,C,S,C,C,T,S,T,S,T,C,S,T,C,S,C,C,T,T,S,T,S]
                in at_complete (AT at1_solved at1_box) `T.assertEqual` True|]),
            $(T.testcase [e|
                let
                  at1 = AT( A.listArray ((One, One), (Six, Six)) [ C,T,B,C,B,B,B,B,B,B,B,C,B,B,B,C,B,B,B,B,S,B,B,B,C,B,B,B,B,B,B,B,T,B,T,S]) at1_box
                  at1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                in at_correct at1 `T.assertEqual` False|]),
            $(T.testcase [e|
                let
                  at1_box = \box -> case box of 1 -> [(One, One), (Two, One), (Three, One)];2 -> [(One, Two), (One, Three), (Two, Two)];3 -> [(One, Four), (One, Five), (Two, Five)];4 -> [(One, Six), (Two, Six), (Three, Six)];5 -> [(Two, Three), (Three, Three), (Four, Three)];6 -> [(Two, Four), (Three, Four), (Four, Four)];7 -> [(Three, Two), (Four, Two), (Five, Two)];8 -> [(Three, Five), (Four, Five), (Five, Five)];9 -> [(Four, One), (Five, One), (Six, One)];10 -> [(Four, Six), (Five, Six), (Six, Six)];11 -> [(Five, Three), (Six, Two), (Six, Three)];12 -> [(Five, Four), (Six, Four), (Six, Five)]
                  at1_solved = A.listArray ((One, One), (Six, Six)) [ C,T,T,C,S,C,S,T,C,S,T,C,T,C,T,C,S,C,C,T,S,T,S,T,C,S,T,C,S,C,C,T,T,S,T,S]
                in at_correct (AT at1_solved at1_box) `T.assertEqual` True|]),
            $(T.testcase [e|ap_sound (AP (A.listArray ((One, One), (Six, Six)) (cycle [S, T])) (const [])) `T.assertEqual` False|]),
            $(T.testcase [e|
              let
                ap1 = AP ( A.listArray ((One, One), (Four, Four))[ C,B,B,B,B,B,S,B,B,B,B,B,B,B,B,B]) ap1_box
                ap1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
              in ap_sound ap1 `T.assertEqual` True|]),
            $(T.testcase [e|
              let
                ap1 = AP ( A.listArray ((One, One), (Four, Four))[ C,B,B,B,B,B,S,B,B,B,B,B,B,B,B,B]) ap1_box
                ap1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
              in ap_complete ap1 `T.assertEqual` False|]),
            $(T.testcase [e|
              let
                ap1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                ap1_solved = A.listArray ((One, One), (Four, Four)) [C,S,C,S,S,C,S,C,C,S,C,S,S,C,S,C]
              in ap_complete (AP ap1_solved ap1_box) `T.assertEqual` True|]),
            $(T.testcase [e|
              let
                ap1 = AP ( A.listArray ((One, One), (Four, Four))[ C,B,B,B,B,B,S,B,B,B,B,B,B,B,B,B]) ap1_box
                ap1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
              in ap_correct ap1 `T.assertEqual` False|]),
            $(T.testcase [e|
              let
                ap1_box = \box -> case box of 1 -> [(One, One), (Two, One)];2 -> [(One, Two), (One, Three)];3 -> [(One, Four), (Two, Four)];4 -> [(Two, Two), (Two, Three)];5 -> [(Three, One), (Four, One)];6 -> [(Three, Two), (Four, Two)];7 -> [(Three, Three), (Three, Four)];8 -> [(Four, Three), (Four, Four)]
                ap1_solved = A.listArray ((One, One), (Four, Four)) [C,S,C,S,S,C,S,C,C,S,C,S,S,C,S,C]
              in ap_correct (AP ap1_solved ap1_box) `T.assertEqual` True|])
          ]
      ]



