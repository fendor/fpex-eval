module Assignment4 where

import Control.Monad

type Nat1        = Integer  -- Natural numbers starting with 1: [1..]
type Numerator   = Nat1
type Denominator = Nat1
type FracNum = (Numerator,Denominator) -- Only pairs with numerator < denominator
type MaxDenominator = Nat1
type MaxDiff        = Nat1
type MaxSummands    = Nat1
type Representation = [Denominator]

greedy :: FracNum -> Representation
greedy = undefined

equals :: Double -> Double -> Bool
equals x y = abs (x - y) < 0.0000001

gen :: FracNum -> MaxDenominator -> [Representation]
gen (n, d) m =
    filter (\x -> sum (map ((1 /) . fromIntegral) x) `equals` (fromIntegral n / fromIntegral d))
    $ filterM (\_ -> [False, True]) $ takeWhile (< m) [(1 :: Integer)..]

gs1 :: FracNum -> MaxDenominator -> [Representation]
gs1 = undefined

gs2 :: FracNum -> MaxDenominator -> Maybe Representation
gs2 = undefined

bt1 :: FracNum -> MaxDenominator -> MaxDiff -> [Representation]
bt1 = undefined

bt2 :: FracNum -> MaxDenominator -> MaxSummands -> [Representation]
bt2 = undefined