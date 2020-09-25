module Assignment1 where

fib :: Int -> Int
fib n = fib' !! n

fib' = 0 : 1 : zipWith (+) fib' (tail fib')

bar :: Int
bar = 1