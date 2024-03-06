longitud :: (Eq a) => [a] -> Integer
longitud a | a == [] = 0
           | otherwise = longitud ( tail a ) + 1 

ultimo :: [a] -> a
ultimo ( x : []) = x 
ultimo ( _ : xs) = ultimo xs

principio :: (Eq a) => [a] -> [a]
principio (x:xs) | longitud xs == 0 = []
                 | otherwise = x : principio xs

reverso :: (Eq a) => [a] -> [a]
reverso x = invertir x (longitud x)
          where invertir x n | n == 0 = []
                             | otherwise = final : resto 
                            where final = ultimo x
                                  resto = invertir (principio x) (n-1)

pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece elem [] = False
pertenece elem (x:xs) | elem == x = True
                      | otherwise = elem `pertenece` xs     

todosIguales :: (Eq t) => [t] -> Bool
todosIguales (x:[]) = True
todosIguales (x:xs) = (x == head xs) && todosIguales xs 

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos (x:[]) = True
todosDistintos (x:xs) | x `pertenece` xs  = False
                      | otherwise = todosDistintos xs 

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos (x:[]) = False
hayRepetidos (x:xs) | x `pertenece` xs  = True
                    | otherwise = hayRepetidos xs

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar a (x:xs) | a /= x = x : (quitar a xs)
                | otherwise = xs 

quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos a (x:xs) | a /= x = x : (quitarTodos a xs)
                     | otherwise = quitarTodos a xs 

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos (x:[]) = [x]
eliminarRepetidos (x:xs) | x `pertenece` xs = eliminarRepetidos (quitarTodos x (x:xs))
                         | otherwise = x : eliminarRepetidos (quitarTodos x xs)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x y = x `incluido` y && y `incluido` x

incluido :: (Eq a) => [a] -> [a] -> Bool 
incluido [] _ = True                
incluido (a:as) (b:bs) | elem a (b:bs) = incluido as (b:bs)  
                       | otherwise = False  

capicua :: (Eq t) => [t] -> Bool
capicua x | x == (reverso x) = True 
          | otherwise = False

sumatoria :: [Integer] -> Integer
sumatoria (x:[]) = x
sumatoria (x:xs) = x + sumatoria xs 


productoria :: [Integer] -> Integer
productoria (x:[]) = x
productoria (x:xs) = x * productoria xs 

maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:xs) | x >= head xs = maximo (x :(tail xs))
              | otherwise = maximo xs

sumarN :: Integer -> [Integer] -> [Integer]  --sumar n a todos
sumarN n (x:[]) = [x + n]
sumarN n (x:xs) = (x+n) : (sumarN n xs)

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = (2*x) : (sumarN x xs) -- o tambien "sumarN x (x:xs)"

sumarElUltimo :: [Integer] -> [Integer] 
sumarElUltimo lx = sumarN (ultimo lx) lx

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | x `mod` 2 == 1 = pares (quitarTodos x xs)
             | otherwise = x : (pares xs)

esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x n = x `mod` n == 0 

multiplosDeN :: Integer -> [Integer] -> [Integer] 
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | x `esMultiploDe` n = x : (multiplosDeN n xs)
                      | otherwise = multiplosDeN n xs

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) | x == maxm = (ordenar xs) ++ [maxm]
               | otherwise = (ordenar (quitar maxm (x:xs))) ++ [maxm]
               where maxm = maximo (x:xs)


sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (x:xs) | (x == ' ') && (head xs) == ' '  = sacarBlancosRepetidos xs
                             | otherwise = x : (sacarBlancosRepetidos xs)

contarPalabras :: [Char] -> Integer
contarPalabras [] = 1
contarPalabras (x:xs) | x == ' ' = 1 + contarPalabras xs 
                      | otherwise = contarPalabras xs

frase = ["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog", "font"]
fox = "the quick brown fox jumps over the lazy dog font"

masLarga :: [[Char]] -> [Char]
masLarga (x:[]) = x
masLarga (x:xs) | (longitud x ) > longitud (head xs) = masLarga (x : (tail xs))
                | otherwise = masLarga xs

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga x = masLarga (palabras x)

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras (x:xs) | x == ' ' = palabras xs
                | otherwise = (agarrar (x:xs)) : (palabras (sacar (x:xs)))
                where agarrar [] = []
                      agarrar (x:xs) | x == ' ' = []
                                     | otherwise = x : (agarrar xs)
                      sacar [] = []
                      sacar (x:xs) | x == ' ' = xs
                                   | otherwise = sacar xs

aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar (x:xs) = x ++ (aplanar xs)

armarEspacios :: Integer -> [Char]
armarEspacios 0 = []
armarEspacios n = ' ' : (armarEspacios (n-1)) 

aplanarConBlancos :: [[Char]] -> [Char]
aplanarConBlancos x = aplanarConNBlancos x 1

aplanarConNBlancos :: [[Char]] -> Integer -> [Char]
aplanarConNBlancos (x:[]) _ = x
aplanarConNBlancos (x:xs) n = (aplanar [x]) ++ (armarEspacios n) ++ (aplanarConNBlancos xs n)

nat2bin :: Integer -> [Integer]
nat2bin 0 = []
nat2bin n = (nat2bin (n `div` 2)) ++ [n `mod` 2]

bin2nat :: [Integer] -> Integer
bin2nat [] = 0
bin2nat (x:xs) = x * (2 ^ (longitud (x:xs) - 1)) + bin2nat xs

nat2hex :: Integer -> [Char]
nat2hex 0 = []
nat2hex n = (nat2hex (n `div` 16)) ++ [hexa (n `mod` 16)]
          where hexa 10 = 'A'
                hexa 11 = 'B'
                hexa 12 = 'C'
                hexa 13 = 'D'
                hexa 14 = 'E'
                hexa 15 = 'F'
                hexa n = head (show n)

sumaAcumulada :: [Integer] -> [Integer]
sumaAcumulada [] = []
sumaAcumulada (x:[]) = [x]
sumaAcumulada (x:xs) = x : (sumarN x (sumaAcumulada xs))

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = (descomponer x) : (descomponerEnPrimos xs)


descomponer :: Integer -> [Integer]
descomponer 1 = []
descomponer n = (menorDiv n) : (descomponer (n `div` (menorDiv n)))

menorDivDesde :: Integer -> Integer -> Integer
menorDivDesde n m | mod n m == 0 = m
                  | otherwise = menorDivDesde n (m + 1)

menorDiv :: Integer -> Integer
menorDiv 1 = 1
menorDiv n = menorDivDesde n 2


type Set a = [a]

agregarATodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
agregarATodos n [] = []
agregarATodos n (x:xs) = (agregar n x) : (agregarATodos n xs)

agregar :: Integer -> Set Integer -> Set Integer
agregar n [] = [n]
agregar n (x:xs) | n == x = (x:xs)
                 | n < x = n : (x:xs)
                 | otherwise = x : (agregar n xs)

partes :: Integer -> Set (Set Integer)
partes 0 = [[]]
partes n = (partes (n-1)) ++ (agregarATodos n (partes (n-1)))

productoCartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productoCartesiano _ [] = []
productoCartesiano [] _ = []
productoCartesiano (x:xs) (y:ys) = (x,y) : (productoCartesiano [x] ys  ++ productoCartesiano xs ys ++  productoCartesiano xs [y])
















