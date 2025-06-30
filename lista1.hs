sumTo :: Integer -> Integer
sumTo 1 = 1
sumTo n = n + sumTo (n-1)

potencia :: Integer -> Integer -> Integer
potencia n 0 = 1
potencia n k = n * potencia n (k -1)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + (binomial (n-1) (k -1))     

