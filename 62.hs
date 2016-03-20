data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r)         = [x] ++ internals l ++ internals r

internals' :: Tree a -> [a]
internals' t = internals'' t []
  where
    internals'' Empty xs                  = xs
    internals'' (Branch _ Empty Empty) xs = xs
    internals'' (Branch x l r) xs         = x : internals'' l (internals'' r xs)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _          = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

atLevel' :: Tree a -> Int -> [a]
atLevel' t n = atLevel'' t n []
  where
    atLevel'' Empty _ xs          = xs
    atLevel'' (Branch x _ _) 1 xs = x:xs
    atLevel'' (Branch _ l r) n xs = atLevel'' r (n-1) (atLevel'' l (n-1) xs)

main :: IO ()
main = print $ atLevel' tree4 2