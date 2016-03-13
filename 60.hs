--import Data.List
--import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

--maxNodes h = floor $ 2 ** h - 1

minNodes :: Integer -> Integer
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + (minNodes (h - 1) + minNodes (h - 2))

maxHeight :: Integer -> Integer
maxHeight n =
  fst $ 
  last $ 
  takeWhile (\(_,ns) -> n >= ns) $ 
  map (\h -> (h, minNodes h)) [0..]

hbalTreeNodes :: a -> Integer -> [Tree a]
hbalTreeNodes _ 0 = [ Empty ]
hbalTreeNodes x n =
  [ Branch x l r | (a,b) <- [ (l,r) | l <- [0..n-1],
                                      r <- [n-1-l],
                                      minNodes (maxHeight l - 1) <= r,
                                      minNodes (maxHeight r - 1) <= l ],
                   l  <- hbalTreeNodes x a,
                   r  <- hbalTreeNodes x b ]

main :: IO ()
main = print $ hbalTreeNodes 'x' 1