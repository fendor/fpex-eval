module Assignment5 where

import Control.Monad
import Test.QuickCheck

fib :: Integer -> Integer
fib = undefined

prop_ccc_1 :: Integer -> Bool
prop_ccc_1 n = undefined

prop_ccc_2 :: Integer -> Property
prop_ccc_2 n = undefined
prop_ccc_3 :: Integer -> Property
prop_ccc_3 n = undefined
prop_ccc_4 :: Integer -> Property
prop_ccc_4 n = undefined
prop_ccc_5 :: Integer -> Property
prop_ccc_5 n = undefined

prop_ccc_3_trivial :: Integer -> Property
prop_ccc_3_trivial = undefined


prop_ccc_3_classify :: Integer -> Property
prop_ccc_3_classify = undefined

prop_ccc_3_collect :: Integer -> Property
prop_ccc_3_collect = undefined

type Threshold = Integer
prop_ccd :: Threshold -> Integer -> Property
prop_ccd t n = undefined

type MaxNumberOfSummands = Integer
type Numerator           = Integer
type Denominator         = Integer

prop_greedy :: MaxNumberOfSummands -> (Numerator,Denominator) -> Property
prop_greedy max (n,d) = undefined

type Stack  = [Integer]

empty :: Stack
empty       = []
is_empty :: Stack -> Bool
is_empty [] = True
is_empty _  = False
push :: Integer -> Stack -> Stack
push x xs   = (x:xs)
pop :: Stack -> Stack
pop []      = error "Stack is empty"
pop (_:xs)  = xs
top :: Stack -> Integer
top []      = error "Stack is empty"
top (x:_)   = x

prop_31 :: Integer -> Stack -> Property
prop_31 n ns = undefined
prop_32 :: Integer -> Stack -> Property
prop_32 n ns = undefined
prop_34 :: Integer -> Stack -> Property
prop_34 n ns = undefined
prop_36 :: Integer -> Stack -> Property
prop_36 n ns = undefined

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = undefined

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

number :: Eq a => Tree a -> Tree Int
number = undefined
number1 :: Eq a => Tree a -> Tree Int
number1 = undefined

prop_rename :: Tree String -> Property
prop_rename = undefined