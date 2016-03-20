data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = build 1
  where 
    build i
      | n < i     = Empty
      | otherwise = Branch 'x' (build $ i*2) (build $ i*2+1)

levels :: Tree a -> [[Bool]]
levels Empty          = [[False]]
levels (Branch a l r) = [True] : zipWith (++) (levels l) (levels r)

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = 
  (all check $ map (\i -> (i, lvls !! i)) [0 .. length lvls - 1]) && 
  (checkLast $ last $ lvls)
  where 
    lvls         = levels t
    check (i,xs) = length xs == 2^i
    checkLast    = all not . dropWhile id

main :: IO ()
main = mapM_ (print . isCompleteBinaryTree . completeBinaryTree) [0..8]