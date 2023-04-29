{-
    Usando recursi ́on, implementar la funci ́on sumaPrimerosNImparesEspecial :: Integer ->Integer, cuya especificaci ́on
    es la siguiente:
        problema sumaPrimerosNImparesEspecial (n: Z) : Z {
            requiere: {n ≥ 1}
            asegura: {res = Σ(i=1, 2n−1) if (i mod 2 = 0) then 0 else 2 ∗ i + 2 fi}
        }
-}

main :: IO()
main = do {
  x <- readLn ;
  print(sumaPrimerosNImpares(x ::(Integer)))
  }

esPar :: Integer -> Bool
esPar n = mod n 2 == 0

sumaPrimerosNImpares :: Integer -> Integer
sumaPrimerosNImpares n = sumaPrimerosNImparesEspecialAux (2*n - 1)


sumaPrimerosNImparesEspecialAux :: Integer -> Integer
sumaPrimerosNImparesEspecialAux 0 = 0
sumaPrimerosNImparesEspecialAux i | esPar i = sumaPrimerosNImparesEspecialAux (i-1)
                                  | otherwise = (2*i + 2) + sumaPrimerosNImparesEspecialAux (i-1)
