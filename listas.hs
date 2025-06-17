import Data.Char (isDigit, GeneralCategory (UppercaseLetter), toUpper, isUpper)
import Distribution.Simple.Utils (xargs)
addPairs :: [(Int, Int)] -> [Int]
addPairs lp = [a + b | (a, b) <- lp]

digits :: String -> String
digits st = [ch | ch <- st, isDigit ch]

paraMaiuscula :: String -> String
paraMaiuscula a = [char | char <- a, isUpper char]

divisores :: Int ->[Int]
divisores k = [x | x <- [1 .. div k 2], mod k x == 0] ++ [k]

firstDigit :: String -> Char
firstDigit st = case digits st of
    [] -> '\0'
    (a:as) -> a

soma :: [Int] -> Int
soma as = foldr (+) 0 as

menorLista :: [Int] ->Int
menorLista [] = minBound :: Int
menorLista [x] = x
menorLista (a : as)
 | a < menorLista as = a
 | otherwise = menorLista as

takefinal :: [a] -> Int -> [a]
takefinal [] n = []
takefinal list 0 = []
takefinal [x] n = [x]
takefinal list n = last list : takefinal (init list) (n-1)

remove :: Int -> [a] -> [a]
remove n list
    | n > length list = error "list index out of range"
    | otherwise = first_half ++ second_half
        where
            first_half = take n list
            second_half = tail (drop n list)

firstInt :: [Int] -> Int
firstInt [] = 0
firstInt list = head list

firstTwosum :: [Int] -> Int
firstTwosum [] = 0
firstTwosum [x] = x
firstTwosum (a : as) = a + head as

produto :: [Int] -> Int
produto as = foldr (*) 1 as

contains :: Int -> [Int] -> Bool
contains x [] = False
contains x (a : as)
 | a == x = True
 | otherwise = contains x as

unique :: [Int] -> [Int]
unique [] = []
unique (a : as)
 | not (contains a as) = a : unique as
 | otherwise = unique as

isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (a : as) = (a < head as) && (isIncreasing as)






