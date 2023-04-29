-- test ej 1
    test :: IO()
    test = do {
        evals :: [(Int, Int, Int)]
        evals = [(1,2,3),(2,3,4),(4,2,1),(4,(-4),4),(6,7,100),(100,(-200),301),(123,123,567),(234,634,1239),(1,-(1),2),(0,0,(-3))]

        res :: [Bool]
        res = [False,False,True,True,True,True,False,True,True,True]

        if (map sumaMenosQueMax evals == res) then print "OK" else print "ERROR";
    }

-- test ej 2
    test :: IO()
    test = do {
        evals :: [Int]
        evals = [0,10,10456,156,1563,41863,48263,4186,786,786,78396,123,504]

        res :: [Int]
        res = [0,1,16,12,15,22,23,19,21,21,33,6,9]

        if (map sumaDigitos evals == res) then print "OK" else print "ERROR";
    }

-- test ej 3

    test :: IO()
    test = do {
        evals :: [Integer]
        evals = [1..10]

        res :: [Integer]
        res = [24,8640,14515200,73156608000,869100503040000,20879270485032960000,912006534786239692800000, 66977759914701443039232000000, 7788173922881483796601896960000000, 1367291813901073295331429030297600000000]
        
        if (map prod evals == res) then print "OK" else print "ERROR";
    }

-- test ej 4

    test :: IO()
    test = do {
        evals :: [Integer]
        evals = [1..20]

        res :: [Integer]
        res = [4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80]

        if (map sumaPrimerosNImparesEspecial evals == res) then print "OK" else print "ERROR";
    }

-- test ej 5

    test :: IO()
    test = do {

        res :: [Integer]
        res = [1,3,5,8,10,14,16,20,23,27,29,35,37,41,45,50,52,58,60,66]
        if (map combinacionesMenoresOIguales [1..20] == res) then print "OK" else print "ERROR";
    }
