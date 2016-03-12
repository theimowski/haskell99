import Data.List (nub)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

instance Functor Tree where
  fmap f t =
    case t of 
      Empty -> Empty
      Branch a l r -> Branch (f a) (fmap f l) (fmap f r)

extend :: a -> Tree (Int,Int,a) -> [Tree (Int,Int,a)]
extend a Empty = [Branch (0,0,a) Empty Empty]
extend a (Branch (nl,nr,x) l r) = extendedL ++ extendedR 
  where
    extendedL = 
      if nl > nr then 
        []
      else
        [ Branch (nl+1,nr,x) l' r | l' <- extend a l ]
    extendedR = 
      if nl < nr then 
        []
      else
        [ Branch (nl,nr+1,x) l r' | r' <- extend a r ]
 

cbalTree :: Int -> [Tree Char]
cbalTree n = nub $ map (fmap (\(_,_,a) -> a)) $ cbal n
  where 
    cbal 0 = [Empty]
    cbal n = do
      smaller <- cbal (n-1)
      extend 'x' smaller

cbalTree2 :: Int -> [Tree Char]
cbalTree2 0 = [Empty]
cbalTree2 n =
  let (q,r) = (n-1) `quotRem` 2 in
  [ Branch 'x' l r | i <- [q .. q+r],
                     l <- cbalTree2 i,
                     r <- cbalTree2 (n - 1 - i)] 
