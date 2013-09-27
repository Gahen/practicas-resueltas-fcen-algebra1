sum1 :: Int -> Int
sum1 1 = 1
sum1 n = ( ( ( 2 * n ) - 1 ) ^ 2 ) + ( sum1 (n - 1) )

sum2 :: Int -> Int
sum2 1 = - 2
sum2 n = ( ( - 1 ) ^ n ) * ( 2 ^ n ) + ( sum2 (n - 1) )

lista1 :: [ Int ] -> [ Int ] -> Int
lista1 [] a = 0
lista (x:xs) y | inList x y = 1 + (lista1 xs y)
lista (x:xs) y | otherwise = lista1 xs y

inList :: Int -> [ Int ] -> Bool
inList y [] = False
inList y (x:xs) | y == x = True
inList y (x:xs) | otherwise = inList y xs
