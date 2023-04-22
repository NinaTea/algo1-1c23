fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Integer
parteEntera x = floor x

esDivisible :: Integer -> Integer -> Bool
esDivisible 0 y = True
esDivisible x y | x < y = False
                | otherwise = esDivisible (x-y) y

medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos x = mod x 10 + sumaDigitos (div x 10)

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales 0 = True
todosDigitosIguales x = mod x 10 == mod (div x 10) 10 && todosDigitosIguales (div x 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito x y = mod (div x (10^y)) 10

esCapicua :: Integer -> Bool
esCapicua x | x == invertir x = True
            where invertir x = invertir' x 0
                  invertir' 0 y = y
                  invertir' x y = invertir' (div x 10) (y*10 + mod x 10)


-- funciones de ejs de algebra (SS 2.4)


f1a :: Integer -> Integer 
f1a 0 = 1 
f1a n = (2 ^ n) + (f1a (n-1))

f2a :: Integer -> Integer -> Integer
f2a n 1 = 1
f2a n q = n ^ q + f2a n (q-1)

f3a :: Integer -> Integer -> Integer
f3a x i = f2a x (2*i)

f4a :: Integer -> Integer -> Integer
f4a n q = f4aAux q n (2*n) 
      where f4aAux b d h | h == d = b ^ h
                         | otherwise = b ^ h + f4aAux b d (h-1) 

-- listo

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

eAprox :: Integer -> Float
eAprox 1 = 2
eAprox n =  (1 / (fromIntegral ((factorial n)))) + eAprox (n-1) 

e :: Float
e = eAprox 100

raizDe2Aprox :: Integer -> Float
raizDe2Aprox 0 = 1
raizDe2Aprox n = (1 / 2) * (raizDe2Aprox (n-1) + 2 / raizDe2Aprox (n-1))

sumatoriaDobleF :: Integer -> Integer -> Integer
sumatoriaDobleF 0 m = 0
sumatoriaDobleF n m = sumatoriaInt n m + sumatoriaDobleF (n-1) m
                  where sumatoriaInt n 0 = n ^ 0
                        sumatoriaInt n m = n ^ m + sumatoriaInt n (m-1)

-- el 14 y 15 no lo voy a hacer por paja

menorDivDesde :: Int -> Int -> Int
menorDivDesde n m | mod n m == 0 = m
                  | otherwise = menorDivDesde n (m + 1)

menorDiv :: Int -> Int
menorDiv 1 = 1
menorDiv n = menorDivDesde n 2


esPrimo :: Int -> Bool
--esPrimo n = sumaDivisores n == (n + 1)
esPrimo n = menorDiv n == n

           
nEsimoPrimoDesde :: Int -> Int -> Int -> Int
nEsimoPrimoDesde n c p | c == n = p - 1
                       | esPrimo p = nEsimoPrimoDesde n (c + 1) (p + 1)
                       | otherwise = nEsimoPrimoDesde n c (p + 1)
                       
nEsimoPrimo :: Int -> Int
nEsimoPrimo n = nEsimoPrimoDesde n 0 2


mcd :: Integer -> Integer -> Integer --Algoritmo euclideo para mcd, recursivo: sean a, b dos numeros enteros
mcd x 0 = x                          --mod a b = r, realizar la funcion de manera recursiva con a = b y b = r, repetir hasta
mcd x y = mcd y (mod x y)            --que de 0


sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = mcd a b == 1 

esFibonacci :: Integer -> Bool
esFibonacci n = esFibonacciDesde n 0 1
            where esFibonacciDesde n a b | n == a = True
                                         | n < a = False
                                         | otherwise = esFibonacciDesde n b (a+b)

mayorDigitoPar :: Integer -> Integer
mayorDigitoPar 0 = 0
mayorDigitoPar n | mod n 2 == 0 = max (mod n 10) (mayorDigitoPar (div n 10))
                 | otherwise = mayorDigitoPar (div n 10)

{-
esSumaInicialDePrimos :: Int ->Bool
esSumaInicialDePrimos n = esSumaInicialDePrimosDesde n 0 2
-}