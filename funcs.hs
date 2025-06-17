poli2 :: Double -> Double -> Double -> Double ->Double -> Double
poli2 a b c d x = a^2*x + b*x + c

parImpar :: Int -> String
parImpar x
 | mod x 2 == 0 = "par"
 | otherwise  = "impar"

quantosIguais :: Integer -> Integer -> Integer -> Integer
quantosIguais x y z
 | x == y && x == z = 3
 | x == y || x == z = 2
 | y == z = 2
 |otherwise = 0

ehZero :: Int -> String
ehZero 0 = "sim"
ehZero x = "nao"

sumTo :: Int -> Int
sumTo 1 = 1
sumTo n = n + sumTo (n-1)

potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n x = n * potencia n (x-1)

bin :: Int -> Int -> Int
bin n 0 = 1
bin 0 k = 0
bin n k = bin (n-1) k + bin (n-1) (k-1)

addEspaços :: Int -> String
addEspaços 0 = ""
addEspaços n = " " ++ addEspaços (n-1)

aDireita :: String -> Int -> String
aDireita x n = x ++ addEspaços n

trib :: Int -> Int
trib 1 = 1
trib 2 = 1
trib 3 = 2
trib x = (fstTuple.fibTrip) x

fstTuple :: (a, a, a) -> a
fstTuple (a, b, c) = a

tribStep :: (Int, Int, Int) -> (Int, Int, Int)
tribStep (a, b, c) = (b, c, a + b + c)

fibTrip :: Int -> (Int, Int, Int)
fibTrip 1 = (1, 1, 2)
fibTrip n = tribStep(fibTrip(n-1))
