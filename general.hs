square :: Int -> Int
square x = x * x

member :: Eq t => [t] -> t -> Bool
member [] b = False
member (a : as) b = (a == b) || member as b

data List t = Nil | Cons t (List t)
                deriving (Eq, Ord, Show)

data Tree t = Nilt |
                Node t ( Tree t) (Tree t)
                deriving (Eq, Ord, Show)


class Info a where
    examples :: [a]
    size :: a -> Int

