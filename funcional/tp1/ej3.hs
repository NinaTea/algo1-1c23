{-
    Implementar la función prod :: Integer ->Integer, cuya especificaci ́on es la siguiente:
    problema prod (n:Z) : Z {
        requiere: {n ≥ 1}
        asegura: {res = Π(i=1, 2n) (i^2 + 2*i)}
    }
-}

main :: IO()
main = do {
  x <- readLn ;
  print(prod(x ::(Integer)))
  }

prod :: Integer -> Integer
prod n = prodAux (2*n)

prodAux :: Integer -> Integer
prodAux 1 = 3
prodAux i = (i^2 + 2*i) * (prodAux (i-1))
