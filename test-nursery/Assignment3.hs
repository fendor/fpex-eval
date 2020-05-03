{-# LANGUAGE ScopedTypeVariables #-}
module Assignment2 where

import Data.Array
import Data.Function
import Data.List

sta :: [Array Integer (Maybe Integer)]
sta = undefined

conv :: Array Integer (Maybe Integer) -> [Integer]
conv = undefined

pretty_print :: [Array Integer (Maybe Integer)] -> [[Integer]]
pretty_print = undefined

kns :: [Integer]
kns = undefined

type Nat0 = Int
type NullsOnes = String -- Only nonempty strings over {`0`,`1`}

mcpc_eq :: ([NullsOnes],[NullsOnes]) -> Nat0 -> Maybe [Int]
mcpc_eq = undefined
-- mcpc_le :: ([NullsOnes],[NullsOnes]) -> Nat0 -> Maybe [Int]
mcpc_le :: ([NullsOnes],[NullsOnes]) -> Nat0 -> Maybe [Int]
mcpc_le (x:xs, y:ys) n = undefined