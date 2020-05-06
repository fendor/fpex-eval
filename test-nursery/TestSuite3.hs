{-# LANGUAGE TemplateHaskell #-}

import Assignment3 hiding (main)
import qualified Data.Array as A
import qualified TestSpec as T
import Prelude hiding (Ordering (..))

main :: IO ()
main =
  T.runTestSuiteWithTestdata 5 [] $
    T.testSuite
      [ T.group
          (T.TestGroupProps "Naive solvers" 5 0 10)
          [ $(T.testcase [e|p_solve_init p_trivial `T.assertEqual` Just p_trivial_solved|]),
            $(T.testcase [e|t_solve_init t_trivial `T.assertEqual` Just t_trivial_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart solvers" 10 0 30)
          [ $(T.testcase [e|p_solve p1 `T.assertEqual` Just p1_solved|]),
            $(T.testcase [e|p_solve p2 `T.assertEqual` Just p2_solved|]),
            $(T.testcase [e|t_solve t1 `T.assertEqual` Just t1_solved|]),
            $(T.testcase [e|t_solve t2 `T.assertEqual` Just t2_solved|])
            --  $(T.testcase [e|t_solve t3 `T.assertEqual` Just t3_solved|])
          ],
        T.group
          (T.TestGroupProps "Naive solvers (array representation)" 5 0 10)
          [ $(T.testcase [e|ap_solve_init ap_trivial `T.assertEqual` Just ap_trivial_solved|]),
            $(T.testcase [e|at_solve_init at_trivial `T.assertEqual` Just at_trivial_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart solvers (array representation)" 10 0 30)
          [ $(T.testcase [e|ap_solve ap1 `T.assertEqual` Just ap1_solved|]),
            $(T.testcase [e|ap_solve ap2 `T.assertEqual` Just ap2_solved|]),
            $(T.testcase [e|at_solve at1 `T.assertEqual` Just at1_solved|]),
            $(T.testcase [e|at_solve at2 `T.assertEqual` Just at2_solved|])
            --  $(T.testcase [e|at_solve at3 `T.assertEqual` Just at3_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart predicates" 1 0 20)
          [ $(T.testcase [e|t_sound (LT [[B]] (const [])) `T.assertEqual` False|]),
            $(T.testcase [e|t_sound (LT (t1_solved ++ []) (const [])) `T.assertEqual` False|]),
            $(T.testcase [e|t_sound t1 `T.assertEqual` True|]),
            $(T.testcase [e|t_complete t1 `T.assertEqual` False|]),
            $(T.testcase [e|t_complete (LT t1_solved t1_box) `T.assertEqual` True|]),
            $(T.testcase [e|t_correct t1 `T.assertEqual` False|]),
            $(T.testcase [e|t_correct (LT t1_solved t1_box) `T.assertEqual` True|]),
            $(T.testcase [e|t_correct (LT t2_solved t1_box) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound (LP [[B]] p1_box) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound (LP (p1_solved ++ []) p1_box) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound p1 `T.assertEqual` True|]),
            $(T.testcase [e|p_complete p1 `T.assertEqual` False|]),
            $(T.testcase [e|p_complete (LP p1_solved p1_box) `T.assertEqual` True|]),
            $(T.testcase [e|p_correct p1 `T.assertEqual` False|]),
            $(T.testcase [e|p_correct (LP p1_solved p1_box) `T.assertEqual` True|]),
            $(T.testcase [e|p_correct (LP p2_solved p1_box) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound (AT (A.listArray ((One, One), (One, One)) [B]) at1_box) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound (AT (A.listArray ((One, One), (Six, Six)) (cycle [S, T])) at1_box) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound at1 `T.assertEqual` True|]),
            $(T.testcase [e|at_complete at1 `T.assertEqual` False|]),
            $(T.testcase [e|at_complete (AT at1_solved at1_box) `T.assertEqual` True|]),
            $(T.testcase [e|at_correct at1 `T.assertEqual` False|]),
            $(T.testcase [e|at_correct (AT at1_solved at1_box) `T.assertEqual` True|]),
            $(T.testcase [e|ap_sound (AP (A.listArray ((One, One), (One, One)) [B]) ap1_box) `T.assertEqual` False|]),
            $(T.testcase [e|ap_sound (AP (A.listArray ((One, One), (Six, Six)) (cycle [S, T])) ap1_box) `T.assertEqual` False|]),
            $(T.testcase [e|ap_sound ap1 `T.assertEqual` True|]),
            $(T.testcase [e|ap_complete ap1 `T.assertEqual` False|]),
            $(T.testcase [e|ap_complete (AP ap1_solved ap1_box) `T.assertEqual` True|]),
            $(T.testcase [e|ap_correct ap1 `T.assertEqual` False|]),
            $(T.testcase [e|ap_correct (AP ap1_solved ap1_box) `T.assertEqual` True|])
          ]
      ]
  where
    p_trivial =
      $( T.testdata
           [e|
             LP
               [ [C, S, B, S],
                 [S, C, S, C],
                 [C, B, C, S],
                 [S, C, S, B]
               ]
               $ \box -> case box of
                 1 -> [(One, One), (Two, One)]
                 2 -> [(One, Two), (One, Three)]
                 3 -> [(One, Four), (Two, Four)]
                 4 -> [(Two, Two), (Two, Three)]
                 5 -> [(Three, One), (Four, One)]
                 6 -> [(Three, Two), (Four, Two)]
                 7 -> [(Three, Three), (Three, Four)]
                 8 -> [(Four, Three), (Four, Four)]
             |]
       )
    p_trivial_solved =
      $( T.testdata
           [e|
             [ [C, S, C, S],
               [S, C, S, C],
               [C, S, C, S],
               [S, C, S, C]
             ]
             |]
       )
    t_trivial =
      $( T.testdata
           [e|
             LT
               [ [B, T, T, C, S, B],
                 [S, T, C, S, T, C],
                 [T, C, T, C, S, C],
                 [C, T, S, T, S, T],
                 [C, S, T, C, S, C],
                 [B, T, T, S, T, B]
               ]
               ( \box -> case box of
                   1 -> [(One, One), (Two, One), (Three, One)]
                   2 -> [(One, Two), (One, Three), (Two, Two)]
                   3 -> [(One, Four), (One, Five), (Two, Five)]
                   4 -> [(One, Six), (Two, Six), (Three, Six)]
                   5 -> [(Two, Three), (Three, Three), (Four, Three)]
                   6 -> [(Two, Four), (Three, Four), (Four, Four)]
                   7 -> [(Three, Two), (Four, Two), (Five, Two)]
                   8 -> [(Three, Five), (Four, Five), (Five, Five)]
                   9 -> [(Four, One), (Five, One), (Six, One)]
                   10 -> [(Four, Six), (Five, Six), (Six, Six)]
                   11 -> [(Five, Three), (Six, Two), (Six, Three)]
                   12 -> [(Five, Four), (Six, Four), (Six, Five)]
               )
             |]
       )
    t_trivial_solved =
      $( T.testdata
           [e|
             [ [C, T, T, C, S, C],
               [S, T, C, S, T, C],
               [T, C, T, C, S, C],
               [C, T, S, T, S, T],
               [C, S, T, C, S, C],
               [C, T, T, S, T, S]
             ]
             |]
       )
    t1 = $( T.testdata [e|LT t1_puzzle  t1_box|])
    t1_puzzle =
      $( T.testdata
           [e|[ [C, T, B, C, B, B],
                 [B, B, B, B, B, C],
                 [B, B, B, C, B, B],
                 [B, B, S, B, B, B],
                 [C, B, B, B, B, B],
                 [B, B, T, B, T, S]
               ] |])
    t1_box = $( T.testdata
           [e|\box -> case box of
                   1 -> [(One, One), (Two, One), (Three, One)]
                   2 -> [(One, Two), (One, Three), (Two, Two)]
                   3 -> [(One, Four), (One, Five), (Two, Five)]
                   4 -> [(One, Six), (Two, Six), (Three, Six)]
                   5 -> [(Two, Three), (Three, Three), (Four, Three)]
                   6 -> [(Two, Four), (Three, Four), (Four, Four)]
                   7 -> [(Three, Two), (Four, Two), (Five, Two)]
                   8 -> [(Three, Five), (Four, Five), (Five, Five)]
                   9 -> [(Four, One), (Five, One), (Six, One)]
                   10 -> [(Four, Six), (Five, Six), (Six, Six)]
                   11 -> [(Five, Three), (Six, Two), (Six, Three)]
                   12 -> [(Five, Four), (Six, Four), (Six, Five)]
             |]
       )
    t1_solved =
      $( T.testdata
           [e|
             [ [C, T, T, C, S, C],
               [S, T, C, S, T, C],
               [T, C, T, C, S, C],
               [C, T, S, T, S, T],
               [C, S, T, C, S, C],
               [C, T, T, S, T, S]
             ]
             |]
       )

    t2 = $( T.testdata [e|LT
      [ [B, B, C, B, S, B],
        [B, C, B, B, B, B],
        [T, B, B, B, C, B],
        [B, T, B, B, B, C],
        [B, B, B, B, T, B],
        [B, T, B, C, B, B]
      ]
      $ \box -> case box of
        1 -> [(One, One), (Two, One), (Three, One)]
        2 -> [(One, Two), (Two, Two), (Three, Two)]
        3 -> [(One, Three), (One, Four), (Two, Three)]
        4 -> [(One, Five), (One, Six), (Two, Six)]
        5 -> [(Two, Four), (Two, Five), (Three, Five)]
        6 -> [(Three, Three), (Three, Four), (Four, Four)]
        7 -> [(Three, Six), (Four, Five), (Four, Six)]
        8 -> [(Four, One), (Four, Two), (Five, Two)]
        9 -> [(Four, Three), (Five, Three), (Six, Three)]
        10 -> [(Five, One), (Six, One), (Six, Two)]
        11 -> [(Five, Four), (Five, Five), (Five, Six)]
        12 -> [(Six, Four), (Six, Five), (Six, Six)]
        |])

    t2_solved = $( T.testdata [e|
      [ [C, T, C, T, S, S],
        [S, C, S, C, C, S],
        [T, S, T, S, C, T],
        [S, T, S, C, S, C],
        [T, C, S, T, T, T],
        [T, T, S, C, C, C]
      ]
        |])

    at_trivial = $( T.testdata [e|AT
      ( A.listArray
          ((One, One), (Six, Six))
          [ C,T,T,C,S,C,S,T,C,S,T,C,B,C,T,C,S,C,B,T,S,T,S,T,B,S,T,C,S,C,B,T,T,S,T,S
          ]
      )
      $ \box -> case box of
        1 -> [(One, One), (Two, One), (Three, One)]
        2 -> [(One, Two), (One, Three), (Two, Two)]
        3 -> [(One, Four), (One, Five), (Two, Five)]
        4 -> [(One, Six), (Two, Six), (Three, Six)]
        5 -> [(Two, Three), (Three, Three), (Four, Three)]
        6 -> [(Two, Four), (Three, Four), (Four, Four)]
        7 -> [(Three, Two), (Four, Two), (Five, Two)]
        8 -> [(Three, Five), (Four, Five), (Five, Five)]
        9 -> [(Four, One), (Five, One), (Six, One)]
        10 -> [(Four, Six), (Five, Six), (Six, Six)]
        11 -> [(Five, Three), (Six, Two), (Six, Three)]
        12 -> [(Five, Four), (Six, Four), (Six, Five)]
        |])

    at_trivial_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Six, Six))
        [ C,T,T,C,S,C,S,T,C,S,T,C,T,C,T,C,S,C,C,T,S,T,S,T,C,S,T,C,S,C,C,T,T,S,T,S]
        |])

    at1 = $( T.testdata [e|AT( A.listArray ((One, One), (Six, Six)) [ C,T,B,C,B,B,B,B,B,B,B,C,B,B,B,C,B,B,B,B,S,B,B,B,C,B,B,B,B,B,B,B,T,B,T,S]) at1_box|])
    at1_box = $( T.testdata [e|\box -> case box of
        1 -> [(One, One), (Two, One), (Three, One)]
        2 -> [(One, Two), (One, Three), (Two, Two)]
        3 -> [(One, Four), (One, Five), (Two, Five)]
        4 -> [(One, Six), (Two, Six), (Three, Six)]
        5 -> [(Two, Three), (Three, Three), (Four, Three)]
        6 -> [(Two, Four), (Three, Four), (Four, Four)]
        7 -> [(Three, Two), (Four, Two), (Five, Two)]
        8 -> [(Three, Five), (Four, Five), (Five, Five)]
        9 -> [(Four, One), (Five, One), (Six, One)]
        10 -> [(Four, Six), (Five, Six), (Six, Six)]
        11 -> [(Five, Three), (Six, Two), (Six, Three)]
        12 -> [(Five, Four), (Six, Four), (Six, Five)]
        |])
    at1_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Six, Six))
        [ C,T,T,C,S,C,S,T,C,S,T,C,T,C,T,C,S,C,C,T,S,T,S,T,C,S,T,C,S,C,C,T,T,S,T,S]
        |])

    at2 = $( T.testdata [e|AT
      ( A.listArray
          ((One, One), (Six, Six))
          [ B,B,C,B,S,B,B,C,B,B,B,B,T,B,B,B,C,B,B,T,B,B,B,C,B,B,B,B,T,B,B,T,B,C,B,B]
      )
      $ \box -> case box of
        1 -> [(One, One), (Two, One), (Three, One)]
        2 -> [(One, Two), (Two, Two), (Three, Two)]
        3 -> [(One, Three), (One, Four), (Two, Three)]
        4 -> [(One, Five), (One, Six), (Two, Six)]
        5 -> [(Two, Four), (Two, Five), (Three, Five)]
        6 -> [(Three, Three), (Three, Four), (Four, Four)]
        7 -> [(Three, Six), (Four, Five), (Four, Six)]
        8 -> [(Four, One), (Four, Two), (Five, Two)]
        9 -> [(Four, Three), (Five, Three), (Six, Three)]
        10 -> [(Five, One), (Six, One), (Six, Two)]
        11 -> [(Five, Four), (Five, Five), (Five, Six)]
        12 -> [(Six, Four), (Six, Five), (Six, Six)]
        |])

    at2_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Six, Six))
        [ C,T,C,T,S,S,S,C,S,C,C,S,T,S,T,S,C,T,S,T,S,C,S,C,T,C,S,T,T,T,T,T,S,C,C,C]
        |])

    p1 = $( T.testdata [e|LP
      [ [C, B, B, B],
        [B, B, S, B],
        [B, B, B, B],
        [B, B, B, C]
      ] p1_box |])
    p1_box = $( T.testdata [e|\box -> case box of
        1 -> [(One, One), (Two, One)]
        2 -> [(One, Two), (One, Three)]
        3 -> [(One, Four), (Two, Four)]
        4 -> [(Two, Two), (Two, Three)]
        5 -> [(Three, One), (Four, One)]
        6 -> [(Three, Two), (Four, Two)]
        7 -> [(Three, Three), (Three, Four)]
        8 -> [(Four, Three), (Four, Four)]
        |])

    p1_solved = $( T.testdata [e|
      [ [C, S, C, S],
        [S, C, S, C],
        [C, S, C, S],
        [S, C, S, C]
      ]
        |])

    p2 = $( T.testdata [e|LP
      [ [B, C, B, B],
        [B, B, B, S],
        [B, B, B, B],
        [C, B, B, B]
      ]
      $ \box -> case box of
        1 -> [(One, One), (One, Two)]
        2 -> [(One, Three), (One, Four)]
        3 -> [(Two, One), (Three, One)]
        4 -> [(Two, Two), (Two, Three)]
        5 -> [(Two, Four), (Three, Four)]
        6 -> [(Three, Two), (Three, Three)]
        7 -> [(Four, One), (Four, Two)]
        8 -> [(Four, Three), (Four, Four)]
        |])

    p2_solved = $( T.testdata [e|[ [S,C,S,C],
                  [C,S,C,S],
                  [S,C,S,C],
                  [C,S,C,S] ]
        |])

    ap_trivial = $( T.testdata [e|AP (A.listArray ((One,One),(Four,Four))
                      [C,S,C,B,
                        S,C,S,B,
                        C,S,C,B,
                        S,C,S,B])

      $ \box -> case box of
        1 -> [(One,One),(Two,One)]
        2 -> [(One,Two),(One,Three)]
        3 -> [(One,Four),(Two,Four)]
        4 -> [(Two,Two),(Two,Three)]
        5 -> [(Three,One),(Four,One)]
        6 -> [(Three,Two),(Four,Two)]
        7 -> [(Three,Three),(Three,Four)]
        8 -> [(Four,Three),(Four,Four)]
        |])

    ap_trivial_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Four, Four))
        [ C,S,C,S,S,C,S,C,C,S,C,S,S,C,S,C]
        |])

    ap1 = $( T.testdata [e|AP( A.listArray ((One, One), (Four, Four))[ C,B,B,B,B,B,S,B,B,B,B,B,B,B,B,B]) ap1_box|])

    ap1_box = $( T.testdata [e|\box -> case box of
        1 -> [(One, One), (Two, One)]
        2 -> [(One, Two), (One, Three)]
        3 -> [(One, Four), (Two, Four)]
        4 -> [(Two, Two), (Two, Three)]
        5 -> [(Three, One), (Four, One)]
        6 -> [(Three, Two), (Four, Two)]
        7 -> [(Three, Three), (Three, Four)]
        8 -> [(Four, Three), (Four, Four)]
        |])
    ap1_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Four, Four))
        [ C,S,C,S,S,C,S,C,C,S,C,S,S,C,S,C]
        |])

    ap2 = $( T.testdata [e|AP
      ( A.listArray
          ((One, One), (Four, Four))
          [ B,C,B,B,B,B,B,S,B,B,B,B,C,B,B,B]
      )
      ap2_box |])

    ap2_box = $( T.testdata [e| \box -> case box of
        1 -> [(One, One), (One, Two)]
        2 -> [(One, Three), (One, Four)]
        3 -> [(Two, One), (Three, One)]
        4 -> [(Two, Two), (Two, Three)]
        5 -> [(Two, Four), (Three, Four)]
        6 -> [(Three, Two), (Three, Three)]
        7 -> [(Four, One), (Four, Two)]
        8 -> [(Four, Three), (Four, Four)]
        |])

    ap2_solved = $( T.testdata [e|
      A.listArray
        ((One, One), (Four, Four))
        [ S,C,S,C,C,S,C,S,S,C,S,C,C,S,C,S]
        |])
