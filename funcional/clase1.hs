absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
           | otherwise = -x

maximoAbsoluto :: Integer -> Integer -> Integer
maximoAbsoluto x y | absoluto x >= absoluto y = x
                   | otherwise = y

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 a b c | x >= y && x >= z = x 
              | y >= x && y >= z = y
              | z >= x && z >= y = z
            where x = absoluto a 
                  y = absoluto b
                  z = absoluto c

algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 a b = a == 0 || b == 0

algunoEs0_pm :: Integer -> Integer -> Bool
algunoEs0_pm _ 0 = True
algunoEs0_pm 0 _ = True

ambos0 :: Integer -> Integer -> Bool
ambos0 x y = x == 0 && y == 0

ambos0_pm :: Integer -> Integer -> Bool
ambos0_pm 0 0 = True
ambos0_pm _ _ = False

mismoIntervalo_w :: Float -> Float -> Bool
mismoIntervalo_w x y = clase x == clase y
                where clase i | i >= 3 = 1
                              | i >= 7 = 2
                              | otherwise = 3 

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | x <= 7 && y <= 7 = True
                   | x > 7 && y > 7 = True
                   | otherwise = False


sumaDistintos_w :: Integer -> Integer -> Integer -> Integer
sumaDistintos_w x y z | hayIguales x y z = undefined
                      | otherwise = x + y + z
                    where hayIguales x y z | x == y = True
                                           | y == z = True
                                           | x == z = True

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x == y || y == z || z == x = undefined
                    | otherwise = x + y + z

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y = mod x y == 0 

digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10

digitoDecenas :: Integer -> Integer
digitoDecenas x = div (mod x 100) 10

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y = mod x y == 0

{-
a*a + a*b*k = 0
a * (a + b*k) = 0
(a + b*k) = 0 

esto significa que a es multiplo de b (o - mult de)

a mod b = 0 
-}

prodInterno :: (Float, Float) -> (Float, Float) -> Float
prodInterno (a, b) (c, d) = a*c + b*d

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a, b) (c, d) = a < c && b < d 

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a, b) (c, d) = sqrt((c-a)^2 + (d-b)^2)

sumaTerna :: (Float, Float, Float) -> Float
sumaTerna (a, b, c) = a + b + c

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x, y, z) p | x /= 0 && (mod x p) == 0 = x + (sumarSoloMultiplos (0, y, z) p)
                               | y /= 0 && (mod y p) == 0 = y + (sumarSoloMultiplos (0, 0, z) p)
                               | z /= 0 && (mod z p) == 0 = z 
                               | otherwise = 0

posPrimerPar :: (Integer, Integer, Integer) -> Integer 
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3 
                       | otherwise = 4

crearPar :: a -> b -> (a,b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b,a)
invertir (x, y) = (y, x)





todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a, b, c) = (fn a) > (gn a) && (fn b) > (gn b) && (fn c) > (gn c)
                      where fn x | x >= 7 = x*x
                                 | otherwise = (2 * x) - 1 
                            gn x | esPar x = div x 2
                                 | otherwise = 3 * x + 1 
                                 where esPar a = mod a 2 == 0



bisiesto :: Integer -> Bool
bisiesto año = not (not (esMultiploDe año 4) || ((esMultiploDe año 100) && not (esMultiploDe año 400)))

distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a, b, c) (x, y, z) = difAbs a x + difAbs b y + difAbs c z
                                   where difAbs p q = abs (p - q)

comparar :: Integer -> Integer -> Integer
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0
             where sumaUltimosDosDigitos x = digitoDecenas x + digitoUnidades x




 
 


                
