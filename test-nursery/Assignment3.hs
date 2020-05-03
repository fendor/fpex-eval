{-# LANGUAGE ScopedTypeVariables #-}
module Assignment3 where

import Data.Array
import Data.Function
import Data.List
import Data.Array

data Symbol = C | S | T | B deriving (Eq,Ord,Enum,Show)
             -- C: Circle, S: Square, T: Triangle, B: Blank

data Index = One | Two | Three | Four | Five | Six
            deriving (Eq,Ord,Enum,Show, Ix)

type OneTo12 = Int -- Only the numbers 1 to 12 (for the 12 boxes of triplets)
type OneTo8  = Int -- Only the numbers 1 to 8 (for the 8 boxes of pairlets)

-- For pairlets only the Index values One thru Four will be used
type Row_ix = Index
type Col_ix = Index

-- (1) Modelling Puzzles as lists of rows

type Matrix a = [Row a]
type Row a    = [a]

-- The prefixes T and P stand for triplet and pairlet, resp.
-- T_Boxes/P_Boxes values define the fields of the
-- 12/8 boxes of triplet/pairlet puzzles
type T_Boxes = (OneTo12 -> [(Row_ix,Col_ix)])
type P_Boxes = (OneTo8 -> [(Row_ix,Col_ix)])

data T_Puzzle = LT (Matrix Symbol) T_Boxes
data P_Puzzle = LP (Matrix Symbol) P_Boxes

-- Information on boxes suppressed for solved puzzles
type T_Puzzle_Solved = Matrix Symbol
type P_Puzzle_Solved = Matrix Symbol

-- (2) Modelling Puzzles as two-dimensional arrays

-- instance Ix Index where...

-- Type names as above but with a prefix A standing for array
type AMatrix i a = Array i a
data AT_Puzzle   = AT (AMatrix (Index,Index) Symbol) T_Boxes
data AP_Puzzle   = AP (AMatrix (Index,Index) Symbol) P_Boxes

-- Information on boxes suppressed for solved puzzles
type AT_Puzzle_Solved = AMatrix (Index,Index) Symbol
type AP_Puzzle_Solved = AMatrix (Index,Index) Symbol

p_solve_init :: P_Puzzle -> Maybe P_Puzzle_Solved
p_solve_init = undefined

t_solve_init :: T_Puzzle -> Maybe T_Puzzle_Solved
t_solve_init = undefined

p_solve :: P_Puzzle -> Maybe P_Puzzle_Solved
p_solve = undefined

t_solve :: T_Puzzle -> Maybe T_Puzzle_Solved
t_solve = undefined

at_solve_init :: AT_Puzzle -> Maybe AT_Puzzle_Solved
at_solve_init = undefined

ap_solve_init :: AP_Puzzle -> Maybe AP_Puzzle_Solved
ap_solve_init = undefined

at_solve :: AT_Puzzle -> Maybe AT_Puzzle_Solved
at_solve = undefined

ap_solve :: AP_Puzzle -> Maybe AP_Puzzle_Solved
ap_solve = undefined


t_sound    :: T_Puzzle -> Bool
t_sound = undefined
p_sound    :: P_Puzzle -> Bool
p_sound = undefined
t_complete :: T_Puzzle -> Bool
t_complete = undefined
p_complete :: P_Puzzle -> Bool
p_complete = undefined
t_correct  :: T_Puzzle -> Bool
t_correct = undefined
p_correct  :: P_Puzzle -> Bool
p_correct = undefined

at_sound    :: AT_Puzzle -> Bool
at_sound = undefined
ap_sound    :: AP_Puzzle -> Bool
ap_sound = undefined
at_complete :: AT_Puzzle -> Bool
at_complete = undefined
ap_complete :: AP_Puzzle -> Bool
ap_complete = undefined
at_correct  :: AT_Puzzle -> Bool
at_correct = undefined
ap_correct  :: AP_Puzzle -> Bool
ap_correct = undefined