import Data.List (sortBy)

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


sortByDesc :: (Ord b) => (a -> b) -> [a] -> [a]
sortByDesc f = sortBy (\x y -> compare (f y) (f x))

sortBy' :: (Ord b) => (a -> b) -> [a] -> [a]
sortBy' f = sortBy (\x y -> compare (f x) (f y))

kcolor :: (Ord a) => Graph a -> [(a,Int)]
kcolor g = kcolor' [] 1 ordered
  where
    Adj a                = graphToAdj g
    ordered              = map fst $ sortByDesc (length . snd) a
    adjs v               = snd $ head $ dropWhile ((/=v) . fst) a
    kcolor' acc _ []     = sortBy' fst acc
    kcolor' acc c (v:vs) = kcolor' (xs ++ acc) (c+1) (reverse left)
      where
        (vs',left) = 
          foldr 
            (\v' (l,r) -> if v' `elem` (l >>= adjs) then (l,v':r) else (v':l,r)) 
            ([v],[]) 
            vs
        xs         = map (\x -> (x,c)) vs'

g1 :: Graph Char
g1 =  Graph ['a','b','c','d','e','f','g','h','i','j'] 
           [('a','b'),('a','e'),('a','f'),('b','c'),
            ('b','g'),('c','d'),('c','h'),('d','e'),
            ('d','i'),('e','j'),('f','h'),('f','i'),
            ('g','i'),('g','j'),('h','j')]

main :: IO ()
main = print $ kcolor g1