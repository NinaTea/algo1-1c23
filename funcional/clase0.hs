f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16
f _ = undefined

g :: Integer -> Integer 
g 8 = 16
g 16 = 4
g 131 = 1
g _ = undefined 

h :: Integer -> Integer
h x = f (g x)

k :: Integer -> Integer
k x = g (f x)