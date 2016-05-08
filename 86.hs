import Data.List (sortBy,partition)

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adj   a = Adj   [(a,[a])]   deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (v:vs) es) = Adj ((v,es >>= f) : zs)
  where
    f (x1,x2)
      | x1 == v   = [x2]
      | x2 == v   = [x1]
      | otherwise = []
    Adj zs = graphToAdj (Graph vs es)


sortBy' :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy' f xs = sortBy (\x y -> compare (f x) (f y)) xs

kcolor :: (Eq a) => Graph a -> [(a,Int)]
kcolor g = kcolor' [] 1 ordered
  where
    Adj a                = graphToAdj g
    ordered              = map fst $ sortBy' (length . snd) a
    adjs v               = snd $ head $ dropWhile ((/=v) . fst) a
    kcolor' acc _ []     = acc
    kcolor' acc c (v:vs) =
      let (adj,nadj) = partition (\x -> x `elem` adjs v) (v:vs) in
      kcolor' ((map (\x -> (x,c)) nadj) ++ acc) (c+1) adj

g1 :: Graph Char
g1 =  Graph ['a','b','c','d','e','f','g','h','i','j'] 
           [('a','b'),('a','e'),('a','f'),('b','c'),
            ('b','g'),('c','d'),('c','h'),('d','e'),
            ('d','i'),('e','j'),('f','h'),('f','i'),
            ('g','i'),('g','j'),('h','j')]

main :: IO ()
main = print $ kcolor g1