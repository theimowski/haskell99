import Data.List

data Graph a = Graph [a] [(a,a)] deriving (Show,Eq)
data Adj   a = Adj   [(a,[a])]   deriving (Show,Eq)
data Edge  a = Edge  [(a,a)]     deriving (Show,Eq)

graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph vs es) = Adj $ map (\v -> (v, concatMap (adj v) es)) vs
  where
    adj v (v1,v2)
      | v == v1   = [v2]
      | v == v2   = [v1]
      | otherwise = []

graphToEdge :: Graph a -> Edge a
graphToEdge (Graph _ xs) = Edge xs

adjToGraph :: Ord a => Adj a -> Graph a
adjToGraph (Adj xs) = Graph (map fst xs) (concatMap graph xs)
  where
    graph (v,vs) = map (\x -> (v,x)) $ filter (>v) vs

adjToEdge :: Ord a => Adj a -> Edge a
adjToEdge (Adj xs) = Edge (concatMap graph xs)
  where
    graph (v,vs) = map (\x -> (v,x)) $ filter (>v) vs

edgeToGraph :: Ord a => Edge a -> Graph a
edgeToGraph (Edge xs) = Graph (sort $ nub $ concatMap (\(x,y) -> [x,y]) xs) xs

edgeToAdj :: Ord a => Edge a -> Adj a
edgeToAdj (Edge xs) = Adj $ map (\x -> (x, concatMap (adj x) xs)) (sort $ nub $ concatMap (\(x,y) -> [x,y]) xs)
  where
    adj v (v1,v2)
      | v == v1   = [v2]
      | v == v2   = [v1]
      | otherwise = []

g :: Graph Char
g = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

a :: Adj Char
a = Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

e :: Edge Char
e = Edge [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

main :: IO ()
main = print $ edgeToAdj e