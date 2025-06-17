differAux :: Eq a => [a] -> [a] -> Int -> Int
differAux [] [] n = -1
differAux l1 l2 (-1) = -1
differAux l1 l2 n
 | (l1 !! n) /= (l2 !! n) = n
 | otherwise = differAux l1 l2 (n-1)




differ :: (Show a, Eq a) => [a] -> [a] -> Maybe String

differ [] [] = Nothing

differ list1 list2 
 | (length list1) /= (length list2) = Just (show (length list1) ++ " /= " ++ show (length list2))
 | x /= -1 = Just ((show (list1 !! x)) ++ "/=" ++ (show (list2 !! x)))
 | otherwise = Nothing 
 where x = (differAux list1 list2 (length list1 -1))



data Vetor = Vetor Integer Integer Integer 
    deriving Show

instance Eq Vetor where
    
    (==) :: Vetor -> Vetor -> Bool
    Vetor x1 x2 x3 == Vetor y1 y2 y3
     | x1 == y1 && x2 == y2 && x3 == y3 = True
     | otherwise = False

instance Num Vetor where
    (+) :: Vetor -> Vetor -> Vetor
    Vetor x1 x2 x3 + Vetor y1 y2 y3 = Vetor (x1 + y1) (x2 + y2) (x3 + y3)
    (-) :: Vetor -> Vetor -> Vetor
    Vetor x1 x2 x3 - Vetor y1 y2 y3 = Vetor (x1 - y1) (x2 - y2) (x3 - y3)
    abs :: Vetor -> Vetor
    abs (Vetor x y z) = Vetor (abs x) (abs y) (abs z)
    signum :: Vetor -> Vetor
    signum (Vetor x y z) = Vetor (signum x) (signum y) (signum z)
   

contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains x [n] = False
contains x (a : as)
 | a == x = True
 | otherwise = contains x as

unique :: Eq a => [a] -> [a]
unique [] = []
unique (a : as)
 | not (contains a as) = a : unique as
 | otherwise = unique as

freq :: Eq a => [a] -> a -> Int
freq [] a = 0
freq (a : as) x
 | a == x = 1 + freq as x
 | otherwise = freq as x


freqs :: Eq a => [a] -> [(a, Int)]
freqs list = [(x, freq list x) | x <- unique list]

data ITree = ILeaf | INode Int ITree ITree
    deriving Show

instance Eq ITree where


