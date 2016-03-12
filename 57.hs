data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

add :: Ord a => a -> Tree a -> Tree a
add x Empty          = Branch x Empty Empty
add x (Branch y l r)
  | x < y     = Branch y (add x l) r
  | otherwise = Branch y l (add x r)

construct :: Ord a => [a] -> Tree a
construct = foldr add Empty . reverse 
