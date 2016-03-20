data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

--leaf :: a -> Tree a
--leaf x = Branch x Empty Empty

countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         =
  countLeaves l + countLeaves r

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r)         = leaves l ++ leaves r

leaves' :: Tree a -> [a]
leaves' t = leaves'' t []
  where
    leaves'' Empty xs                  = xs
    leaves'' (Branch x Empty Empty) xs = x:xs
    leaves'' (Branch _ l r) xs         = leaves'' r (leaves'' l xs)

main :: IO ()
main = print $ leaves' tree4 