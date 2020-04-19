module Assignment0 where

type Nat0 = Integer

factorial :: Nat0 -> Nat0
factorial n = product [1..n]

fibonacci :: Nat0 -> Nat0
fibonacci n = fibs !! fromInteger n
    where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

binom :: Nat0 -> Nat0 -> Nat0
binom n k = factorial n `div` (factorial k * factorial (n - k))

fac_faulty :: Nat0 -> Nat0
fac_faulty n
    | n < 0   = fac_faulty (-n)
    | n == 0  = 1
    | n <= 10 = n * fac_faulty (n-1)
    | n <= 15 = n + fac_faulty (n-1)
    | True = fac_faulty 42

fib_faulty 0 = 0
fib_faulty 1 = 1
fib_faulty n = fib_faulty (n-2) * fib_faulty (n-1)

binom_faulty :: Nat0 -> Nat0 -> Nat0
binom_faulty n k  = div (factorial n) (factorial k * fac_faulty (n-k))

-- Enforcing time-outs by the test system
fib_slow :: Nat0 -> Nat0
fib_slow 0 = 0
fib_slow 1 = 1
fib_slow n = fib_slow (n-2) + fib_slow (n-1)