

fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-2)


myMap _ [] = []
myMap f (x:xs) = f x : map f xs