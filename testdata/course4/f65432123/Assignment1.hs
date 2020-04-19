module Assignment1 where

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

factorial 0 = 1
factorial n = n * factorial (n-1)

myMap _ [] = []
myMap f (x:xs) = f x : map f xs

main :: IO ()
main = return ()