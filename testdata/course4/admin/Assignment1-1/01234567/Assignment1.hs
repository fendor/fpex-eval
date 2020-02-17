module Assignment1 where


myMap _ [] = []
myMap f (x:xs) = f x : map f xs