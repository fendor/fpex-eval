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
          [ $(T.testcase [e|p_solve_init p_trivial `T.assertEqual` Just p_trivial_solved|]),
            $(T.testcase [e|t_solve_init t_trivial `T.assertEqual` Just t_trivial_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart solvers" 5 0 25)
          [ $(T.testcase [e|p_solve p1 `T.assertEqual` Just p1_solved|]),
            $(T.testcase [e|p_solve p2 `T.assertEqual` Just p2_solved|]),
            $(T.testcase [e|t_solve t1 `T.assertEqual` Just t1_solved|]),
            $(T.testcase [e|t_solve t2 `T.assertEqual` Just t2_solved|]),
            $(T.testcase [e|t_solve t3 `T.assertEqual` Just t3_solved|])
          ],
        T.group
          (T.TestGroupProps "Naive solvers (array representation)" 5 0 10)
          [ $(T.testcase [e|ap_solve_init ap_trivial `T.assertEqual` Just ap_trivial_solved|]),
            $(T.testcase [e|at_solve_init at_trivial `T.assertEqual` Just at_trivial_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart solvers (array representation)" 5 0 25)
          [ $(T.testcase [e|ap_solve ap1 `T.assertEqual` Just ap1_solved|]),
            $(T.testcase [e|ap_solve ap2 `T.assertEqual` Just ap2_solved|]),
            $(T.testcase [e|at_solve at1 `T.assertEqual` Just at1_solved|]),
            $(T.testcase [e|at_solve at2 `T.assertEqual` Just at2_solved|]),
            $(T.testcase [e|at_solve at3 `T.assertEqual` Just at3_solved|])
          ],
        T.group
          (T.TestGroupProps "Smart predicates" 1 0 30)
          [ $(T.testcase [e|t_sound (set_tpuzzle t1 [[B]]) `T.assertEqual` False|]),
            $(T.testcase [e|t_sound (set_tpuzzle t1 (t1_solved ++ [])) `T.assertEqual` False|]),
            $(T.testcase [e|t_sound t1 `T.assertEqual` True|]),
            $(T.testcase [e|t_complete t1 `T.assertEqual` False|]),
            $(T.testcase [e|t_complete (set_tpuzzle t1 t1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|t_correct t1 `T.assertEqual` False|]),
            $(T.testcase [e|t_correct (set_tpuzzle t1 t1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|t_correct (set_tpuzzle t1 t2_solved) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound (set_ppuzzle p1 [[B]]) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound (set_ppuzzle p1 (p1_solved ++ [])) `T.assertEqual` False|]),
            $(T.testcase [e|p_sound p1 `T.assertEqual` True|]),
            $(T.testcase [e|p_complete p1 `T.assertEqual` False|]),
            $(T.testcase [e|p_complete (set_ppuzzle p1 p1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|p_correct p1 `T.assertEqual` False|]),
            $(T.testcase [e|p_correct (set_ppuzzle p1 p1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|p_correct (set_ppuzzle p1 p2_solved) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound (set_atpuzzle at1 (A.listArray ((One, One), (One, One)) [B])) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound (set_atpuzzle at1 (A.listArray ((One, One), (Six, Six)) (cycle [S, T]))) `T.assertEqual` False|]),
            $(T.testcase [e|at_sound at1 `T.assertEqual` True|]),
            $(T.testcase [e|at_complete at1 `T.assertEqual` False|]),
            $(T.testcase [e|at_complete (set_atpuzzle at1 at1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|at_correct at1 `T.assertEqual` False|]),
            $(T.testcase [e|at_correct (set_atpuzzle at1 at1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|ap_sound (set_appuzzle ap1 (A.listArray ((One, One), (One, One)) [B])) `T.assertEqual` False|]),
            $(T.testcase [e|ap_sound (set_appuzzle ap1 (A.listArray ((One, One), (Six, Six)) (cycle [S, T]))) `T.assertEqual` False|]),
            $(T.testcase [e|ap_sound ap1 `T.assertEqual` True|]),
            $(T.testcase [e|ap_complete ap1 `T.assertEqual` False|]),
            $(T.testcase [e|ap_complete (set_appuzzle ap1 ap1_solved) `T.assertEqual` True|]),
            $(T.testcase [e|ap_correct ap1 `T.assertEqual` False|]),
            $(T.testcase [e|ap_correct (set_appuzzle ap1 ap1_solved) `T.assertEqual` True|])
          ]
      ]

set_tpuzzle :: T_Puzzle -> T_Puzzle_Solved -> T_Puzzle
set_tpuzzle (LT _ box) s = LT s box

set_atpuzzle :: AT_Puzzle -> AT_Puzzle_Solved -> AT_Puzzle
set_atpuzzle (AT _ box) s = AT s box

set_ppuzzle :: P_Puzzle -> P_Puzzle_Solved -> P_Puzzle
set_ppuzzle (LP _ box) s = LP s box

set_appuzzle :: AP_Puzzle -> AP_Puzzle_Solved -> AP_Puzzle
set_appuzzle (AP _ box) s = AP s box

t_trivial :: T_Puzzle
t_trivial =
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

t_trivial_solved :: T_Puzzle_Solved
t_trivial_solved =
  [ [C, T, T, C, S, C],
    [S, T, C, S, T, C],
    [T, C, T, C, S, C],
    [C, T, S, T, S, T],
    [C, S, T, C, S, C],
    [C, T, T, S, T, S]
  ]

t1 :: T_Puzzle
t1 =
  LT
    [ [C, T, B, C, B, B],
      [B, B, B, B, B, C],
      [B, B, B, C, B, B],
      [B, B, S, B, B, B],
      [C, B, B, B, B, B],
      [B, B, T, B, T, S]
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

t1_solved :: T_Puzzle_Solved
t1_solved =
  [ [C, T, T, C, S, C],
    [S, T, C, S, T, C],
    [T, C, T, C, S, C],
    [C, T, S, T, S, T],
    [C, S, T, C, S, C],
    [C, T, T, S, T, S]
  ]

t2 :: T_Puzzle
t2 = LT
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

t2_solved :: T_Puzzle_Solved
t2_solved =
  [ [C, T, C, T, S, S],
    [S, C, S, C, C, S],
    [T, S, T, S, C, T],
    [S, T, S, C, S, C],
    [T, C, S, T, T, T],
    [T, T, S, C, C, C]
  ]

t3 :: T_Puzzle
t3 = LT
  [ [T, B, B, B, B, C],
    [B, B, S, C, B, B],
    [B, S, B, B, T, B],
    [B, C, B, B, C, B],
    [B, B, S, C, B, B],
    [C, B, B, B, B, S]
  ]
  $ \box -> case box of
    1 -> [(One, One), (One, Two), (Two, One)]
    2 -> [(One, Three), (One, Four), (Two, Four)]
    3 -> [(One, Five), (Two, Five), (Three, Five)]
    4 -> [(One, Six), (Two, Six), (Three, Six)]
    5 -> [(Two, Two), (Two, Three), (Three, Three)]
    6 -> [(Three, One), (Three, Two), (Four, One)]
    7 -> [(Three, Four), (Four, Four), (Five, Four)]
    8 -> [(Four, Two), (Four, Three), (Five, Two)]
    9 -> [(Four, Five), (Five, Five), (Six, Five)]
    10 -> [(Four, Six), (Five, Six), (Six, Six)]
    11 -> [(Five, One), (Six, One), (Six, Two)]
    12 -> [(Five, Three), (Six, Three), (Six, Four)]

t3_solved :: T_Puzzle_Solved
t3_solved =
  [ [T, T, C, C, T, C],
    [T, C, S, C, T, C],
    [C, S, T, S, T, C],
    [T, C, C, T, C, T],
    [S, C, S, C, S, C],
    [C, T, S, S, T, S]
  ]

at_trivial :: AT_Puzzle
at_trivial = AT
  ( A.listArray ((One,One),(Six,Six))
    [ C,T,T,C,S,C,
      S,T,C,S,T,C,
      B,C,T,C,S,C,
      B,T,S,T,S,T,
      B,S,T,C,S,C,
      B,T,T,S,T,S
    ])

  $ \box -> case box of
    1 -> [(One,One),(Two,One),(Three,One)]
    2 -> [(One,Two),(One,Three),(Two,Two)]
    3 -> [(One,Four),(One,Five),(Two,Five)]
    4 -> [(One,Six),(Two,Six),(Three,Six)]
    5 -> [(Two,Three),(Three,Three),(Four,Three)]
    6 -> [(Two,Four),(Three,Four),(Four,Four)]
    7 -> [(Three,Two),(Four,Two),(Five,Two)]
    8 -> [(Three,Five),(Four,Five),(Five,Five)]
    9 -> [(Four,One),(Five,One),(Six,One)]
    10 -> [(Four,Six),(Five,Six),(Six,Six)]
    11 -> [(Five,Three),(Six,Two),(Six,Three)]
    12 -> [(Five,Four),(Six,Four),(Six,Five)]

at_trivial_solved :: AT_Puzzle_Solved
at_trivial_solved = A.listArray ((One,One),(Six,Six))
  [ C,T,T,C,S,C,
    S,T,C,S,T,C,
    T,C,T,C,S,C,
    C,T,S,T,S,T,
    C,S,T,C,S,C,
    C,T,T,S,T,S
  ]

at1 :: AT_Puzzle
at1 = AT
  ( A.listArray ((One,One),(Six,Six))
    [C,T,B,C,B,B,
    B,B,B,B,B,C,
    B,B,B,C,B,B,
    B,B,S,B,B,B,
    C,B,B,B,B,B,
    B,B,T,B,T,S])

  $ \box -> case box of
    1 -> [(One,One),(Two,One),(Three,One)]
    2 -> [(One,Two),(One,Three),(Two,Two)]
    3 -> [(One,Four),(One,Five),(Two,Five)]
    4 -> [(One,Six),(Two,Six),(Three,Six)]
    5 -> [(Two,Three),(Three,Three),(Four,Three)]
    6 -> [(Two,Four),(Three,Four),(Four,Four)]
    7 -> [(Three,Two),(Four,Two),(Five,Two)]
    8 -> [(Three,Five),(Four,Five),(Five,Five)]
    9 -> [(Four,One),(Five,One),(Six,One)]
    10 -> [(Four,Six),(Five,Six),(Six,Six)]
    11 -> [(Five,Three),(Six,Two),(Six,Three)]
    12 -> [(Five,Four),(Six,Four),(Six,Five)]

at1_solved :: AT_Puzzle_Solved
at1_solved = A.listArray ((One,One),(Six,Six))
  [ C,T,T,C,S,C,
    S,T,C,S,T,C,
    T,C,T,C,S,C,
    C,T,S,T,S,T,
    C,S,T,C,S,C,
    C,T,T,S,T,S
  ]

at2:: AT_Puzzle
at2 = AT (A.listArray ((One,One),(Six,Six))
                   [B,B,C,B,S,B,
                    B,C,B,B,B,B,
                    T,B,B,B,C,B,
                    B,T,B,B,B,C,
                    B,B,B,B,T,B,
                    B,T,B,C,B,B])

     $   \box -> case box of
                1 -> [(One,One),(Two,One),(Three,One)]
                2 -> [(One,Two),(Two,Two),(Three,Two)]
                3 -> [(One,Three),(One,Four),(Two,Three)]
                4 -> [(One,Five),(One,Six),(Two,Six)]
                5 -> [(Two,Four),(Two,Five),(Three,Five)]
                6 -> [(Three,Three),(Three,Four),(Four,Four)]
                7 -> [(Three,Six),(Four,Five),(Four,Six)]
                8 -> [(Four,One),(Four,Two),(Five,Two)]
                9 -> [(Four,Three),(Five,Three),(Six,Three)]
                10 -> [(Five,One),(Six,One),(Six,Two)]
                11 -> [(Five,Four),(Five,Five),(Five,Six)]
                12 -> [(Six,Four),(Six,Five),(Six,Six)]

at2_solved :: AT_Puzzle_Solved
at2_solved = A.listArray ((One,One),(Six,Six))
                       [C,T,C,T,S,S,
                        S,C,S,C,C,S,
                        T,S,T,S,C,T,
                        S,T,S,C,S,C,
                        T,C,S,T,T,T,
                        T,T,S,C,C,C]

at3:: AT_Puzzle
at3 = AT
  (A.listArray ((One,One),(Six,Six))
    [ T,B,B,B,B,C,
      B,B,S,C,B,B,
      B,S,B,B,T,B,
      B,C,B,B,C,B,
      B,B,S,C,B,B,
      C,B,B,B,B,S
    ]
  )

  $ \box -> case box of
    1 -> [(One,One),(One,Two),(Two,One)]
    2 -> [(One,Three),(One,Four),(Two,Four)]
    3 -> [(One,Five),(Two,Five),(Three,Five)]
    4 -> [(One,Six),(Two,Six),(Three,Six)]
    5 -> [(Two,Two),(Two,Three),(Three,Three)]
    6 -> [(Three,One),(Three,Two),(Four,One)]
    7 -> [(Three,Four),(Four,Four),(Five,Four)]
    8 -> [(Four,Two),(Four,Three),(Five,Two)]
    9 -> [(Four,Five),(Five,Five),(Six,Five)]
    10 -> [(Four,Six),(Five,Six),(Six,Six)]
    11 -> [(Five,One),(Six,One),(Six,Two)]
    12 -> [(Five,Three),(Six,Three),(Six,Four)]

at3_solved :: AT_Puzzle_Solved
at3_solved = A.listArray ((One,One),(Six,Six))
                       [T,T,C,C,T,C,
                        T,C,S,C,T,C,
                        C,S,T,S,T,C,
                        T,C,C,T,C,T,
                        S,C,S,C,S,C,
                        C,T,S,S,T,S]

p_trivial :: P_Puzzle
p_trivial = LP
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

p_trivial_solved :: P_Puzzle_Solved
p_trivial_solved =
  [ [C, S, C, S],
    [S, C, S, C],
    [C, S, C, S],
    [S, C, S, C]
  ]

p1 :: P_Puzzle
p1 = LP [ [C,B,B,B],
          [B,B,S,B],
          [B,B,B,B],
          [B,B,B,C] ]

  $ \box -> case box of
    1 -> [(One,One),(Two,One)]
    2 -> [(One,Two),(One,Three)]
    3 -> [(One,Four),(Two,Four)]
    4 -> [(Two,Two),(Two,Three)]
    5 -> [(Three,One),(Four,One)]
    6 -> [(Three,Two),(Four,Two)]
    7 -> [(Three,Three),(Three,Four)]
    8 -> [(Four,Three),(Four,Four)]

p1_solved :: P_Puzzle_Solved
p1_solved = [ [C,S,C,S],
              [S,C,S,C],
              [C,S,C,S],
              [S,C,S,C] ]

p2 :: P_Puzzle
p2 = LP [ [B,C,B,B],
          [B,B,B,S],
          [B,B,B,B],
          [C,B,B,B] ]

  $ \box -> case box of
    1 -> [(One,One),(One,Two)]
    2 -> [(One,Three),(One,Four)]
    3 -> [(Two,One),(Three,One)]
    4 -> [(Two,Two),(Two,Three)]
    5 -> [(Two,Four),(Three,Four)]
    6 -> [(Three,Two),(Three,Three)]
    7 -> [(Four,One),(Four,Two)]
    8 -> [(Four,Three),(Four,Four)]

p2_solved :: P_Puzzle_Solved
p2_solved = [ [S,C,S,C],
              [C,S,C,S],
              [S,C,S,C],
              [C,S,C,S] ]

ap_trivial:: AP_Puzzle
ap_trivial = AP (A.listArray ((One,One),(Four,Four))
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

ap_trivial_solved :: AP_Puzzle_Solved
ap_trivial_solved = A.listArray ((One,One),(Four,Four))
                       [C,S,C,S,
                        S,C,S,C,
                        C,S,C,S,
                        S,C,S,C]

ap1 :: AP_Puzzle
ap1 = AP (A.listArray ((One,One),(Four,Four))
                   [C,B,B,B,
                    B,B,S,B,
                    B,B,B,B,
                    B,B,B,B])

  $ \box -> case box of
    1 -> [(One,One),(Two,One)]
    2 -> [(One,Two),(One,Three)]
    3 -> [(One,Four),(Two,Four)]
    4 -> [(Two,Two),(Two,Three)]
    5 -> [(Three,One),(Four,One)]
    6 -> [(Three,Two),(Four,Two)]
    7 -> [(Three,Three),(Three,Four)]
    8 -> [(Four,Three),(Four,Four)]

ap1_solved :: AP_Puzzle_Solved
ap1_solved = A.listArray ((One,One),(Four,Four))
                       [C,S,C,S,
                        S,C,S,C,
                        C,S,C,S,
                        S,C,S,C]

ap2 :: AP_Puzzle
ap2 = AP (A.listArray ((One,One),(Four,Four))
                   [B,C,B,B,
                    B,B,B,S,
                    B,B,B,B,
                    C,B,B,B])

  $ \box -> case box of
    1 -> [(One,One),(One,Two)]
    2 -> [(One,Three),(One,Four)]
    3 -> [(Two,One),(Three,One)]
    4 -> [(Two,Two),(Two,Three)]
    5 -> [(Two,Four),(Three,Four)]
    6 -> [(Three,Two),(Three,Three)]
    7 -> [(Four,One),(Four,Two)]
    8 -> [(Four,Three),(Four,Four)]

ap2_solved :: AP_Puzzle_Solved
ap2_solved = A.listArray ((One,One),(Four,Four))
                       [S,C,S,C,
                        C,S,C,S,
                        S,C,S,C,
                        C,S,C,S]

ap3 :: AP_Puzzle
ap3 = AP (A.listArray ((One,One),(Four,Four))
                   [B,B,B,C,
                    C,B,B,B,
                    B,B,S,B,
                    B,B,B,B])

  $ \box -> case box of
    1 -> [(One,One),(One,Two)]
    2 -> [(One,Three),(Two,Three)]
    3 -> [(One,Four),(Two,Four)]
    4 -> [(Two,One),(Two,Two)]
    5 -> [(Three,One),(Four,One)]
    6 -> [(Three,Two),(Three,Three)]
    7 -> [(Three,Four),(Four,Four)]
    8 -> [(Four,Two),(Four,Three)]

ap3_solved :: AP_Puzzle_Solved
ap3_solved = A.listArray ((One,One),(Four,Four))
                      [S,C,S,C,
                       C,S,C,S,
                       S,C,S,C,
                       C,S,C,S]