{-
    Ejercicio 1
    Implementar la función sumaMenosQueMax :: (Int, Int, Int) ->Bool, cuya especificaci ́on es la siguiente:

    problema sumaMenosQueMax (t: Z × Z × Z) : Bool {
        requiere: {true}
        asegura: {res = true ⇔ max(t0, t1, t2) > min(t0, t1, t2) + medio(t0, t1, t2)}
    }

    problema max (a, b, c: Z) : Z {
        requiere: {true}
        asegura: {res es el m ́aximo valor entre a, b y c }
    }

    problema min (a, b, c: Z) : Z {
        requiere: {true}
        asegura: {res es el m ́ınimo valor entre a, b y c }
    }

    problema medio (a, b, c: Z) : Z {
        requiere: {true}
        asegura: {(∃l : seq⟨Z⟩)(|l| = 3 ∧ esP ermutacion(l, < a, b, c >) ∧ l[0] ≤ l[1] ≤ l[2] ∧ res = l[1])}
    }

    pred esPermutacion (l1, l2:seq⟨Z⟩) {
        (∀x : Z)cantidadDeApariciones(l1, x) = cantidadDeApariciones(l2, x)
    }
-}


main :: IO ()
main = do
  x <- readLn
  print (sumaMenosQueMax (x :: (Int, Int, Int)))

mins :: (Int, Int, Int) -> Int
mins (a, b, c) = minimo a b c
             where minimo a b c | a < b && a < c = a
                                | b < a && b < c = b
                                | otherwise = c

maxs :: (Int, Int, Int) -> Int
maxs (a, b, c) = maximo a b c
             where maximo a b c | a > b && a > c = a
                                | b > a && b > c = b
                                | otherwise = c
meds :: (Int, Int, Int) -> Int
meds (a, b, c) = medio a b c
             where medio a b c | a > b && a < c = a
                               | a < b && a > c = a
                               | b > a && b < c = b
                               | b < a && b > c = b
                               | otherwise = c

sumaMenosQueMax :: (Int, Int, Int) -> Bool
sumaMenosQueMax t = (mins t + meds t) < maxs t 



