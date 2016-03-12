data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

leaf a = Branch a Empty Empty

mirror :: Tree a -> Tree a
mirror Empty = Empty
mirror (Branch a l r) = Branch a (mirror r) (mirror l)

cbalTrees :: Integer -> [Tree Char]
cbalTrees 0 = [Empty]
cbalTrees n =
  let (q,r) = (n - 1) `quotRem` 2 in 
    [ Branch 'x' l r | 
        i <- [q .. q + r],
        l <- cbalTrees i,
        r <- cbalTrees (n-1-i) ] 

symCbalTrees :: Integer -> [Tree Char]
symCbalTrees 0 = [Empty]
symCbalTrees n =
  case ((n - 1) `mod` 2) of
    1 -> []
    0 -> [ Branch 'x' c (mirror c)  | c <- cbalTrees ((n - 1) `quot` 2) ]
