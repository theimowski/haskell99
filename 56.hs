data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = 
  mirror l1 r2 && mirror r1 l2
mirror _ _ = False


symmetric :: Tree a -> Bool
symmetric t = mirror t t
