-- Ejercicio 1 --
sum1 :: Int -> Int
sum1 1 = 1
sum1 n = ( ( ( 2 * n ) - 1 ) ^ 2 ) + ( sum1 (n - 1) )

-- Ejercicio 2 --
sum2 :: Int -> Int
sum2 1 = - 2
sum2 n = ( ( - 1 ) ^ n ) * ( 2 ^ n ) + ( sum2 (n - 1) )

-- Ejercicio 3 --
lista1:: [Int] -> [Int] -> Int
lista1 [] _ = 0
lista1 (x:xs) ys | estaen x ys = 1 + lista1 xs ys
    	 | otherwise = lista1 xs ys

estaen:: Int -> [Int] -> Bool
estaen _ [] = False
estaen y (x:xs) | y == x = True
		| otherwise = estaen y xs

-- Ejercicio 4 --
triangular:: [Int] -> Bool
triangular [] = True
triangular [x] = True
triangular (x:y:xs) | x <= y = triangular (y:xs)
    	   | x > y = decreciente (y:xs)
		   
		   
decreciente:: [Int] -> Bool
decreciente [] = True
decreciente [x] = True
decreciente (x:y:xs) | x >= y = decreciente (y:xs)
		     | otherwise = False

-- Ejercicio 5 --
factorizar :: Int -> [ Int ]
factorizar n = subfactorizar n (n - 1)

subfactorizar :: Int -> Int -> [ Int ]
-- Caso base, n no tiene divisores entre n-1 y 1, así que es primo y lo devuelvo.
subfactorizar n 1 = [ n ]
-- Si m divide a n entonces factorizo n y la lista de factores que me de la sumo a la lista de factores de lo que me queda.
-- Ejemplo 20 10: factorizo a 10 y a eso le sumo la factorizacion de 2 (20/10). La factorización de 10 es 5 y 2, así que me queda [5,2] ++ [2].
subfactorizar n m | 0 == (n `mod` m) =  (subfactorizar (n `div` m) ((n `div` m) - 1) ) ++ (subfactorizar m (m - 1))
-- Si m no divide lo reduzco
subfactorizar n m | otherwise = subfactorizar n (m - 1)
