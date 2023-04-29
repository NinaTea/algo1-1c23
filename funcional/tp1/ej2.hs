{-
    Implementar la funci ́on sumaDigitos :: Integer ->Integer, cuya especificaci ́on es la siguiente:
    problema sumaDigitos (n:Z) : Z {
        requiere: {n ≥ 0}
        asegura: {res es la suma de todos los d ́ıgitos de n}
    }
-}

main :: IO()
main = do {
  x <- readLn ;
  print(sumaDigitos(x ::(Int)))
  }

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)  


