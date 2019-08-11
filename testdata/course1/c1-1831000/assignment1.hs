

fib 0 = 1
fib 1 = 1x
fib n = fib (n-2) + fib (n-2)

factorial 0 = 1
factorial n = n * factorial (n-1)

myMap _ [] = []
myMap f (x:xs) = f x : map f xs