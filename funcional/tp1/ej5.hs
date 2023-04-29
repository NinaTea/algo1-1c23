{-
    Implementar la funci ́on combinacionesMenoresOIguales :: Integer ->Integer, cuya especificaci ́on es la siguiente:
    problema combinacionesMenoresOIguales (n: Z) : Z {
        requiere: {n ≥ 1}
        asegura: {res =  Σ(i=1, n) Σ(j=1, n) if i ∗ j ≤ n then 1 else 0 fi}
    }
-}


main :: IO()
main = do {
  x <- readLn ;
  print(combinacionesMenoresOiguales(x ::(Integer)))
  }

valor :: Integer -> Integer -> Integer -> Integer
valor i j n | i*j <= n = 1
            | otherwise = 0 

combinacionesMenoresOiguales :: Integer -> Integer
combinacionesMenoresOiguales n = aux1 n n n

aux1 :: Integer -> Integer -> Integer -> Integer
aux1 _ 0 _ = 0 
aux1 n i j = (aux2 n i j) + (aux1 n (i-1) j)

aux2 :: Integer -> Integer -> Integer -> Integer
aux2 _ _ 0 = 0
aux2 n i j = (valor i j n) + (aux2 n i (j-1))

