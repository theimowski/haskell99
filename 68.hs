import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree :: Tree Char
tree = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

treeToPreorder :: Tree a -> [a]
treeToPreorder Empty          = []
treeToPreorder (Branch a l r) = [a] ++ treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree a -> [a]
treeToInorder Empty = []
treeToInorder (Branch a l r) = treeToInorder l ++ a : treeToInorder r

preInTree :: Eq a => [a] -> [a] -> Tree a
preInTree [] []     = Empty
preInTree (x:xs) ys = Branch x l r
  where
    (Just i) = elemIndex x ys
    l        = preInTree (take i xs) (take i ys)
    r        = preInTree (drop i xs) (drop (i+1) ys)


main :: IO ()
main = print $ tree == preInTree (treeToPreorder tree) (treeToInorder tree)