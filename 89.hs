type Node a  = a
type Edge a  = (Node a, Node a)
type Graph a = ([Node a], [Edge a])

adj :: (Eq a) => Graph a -> Node a -> [Node a]
adj (_,es) n = es >>= f
  where
    f (x,y)
      | x == n    = [y]
      | y == n    = [x]
      | otherwise = []

dfs' :: (Eq a) => Graph a -> [Node a] -> [Node a]
dfs' ([],_) _ = []
dfs' _     [] = []
dfs' g@(ns,es) (x:xs)
  | x `notElem` ns = dfs' g xs
  | otherwise   = x : dfs' (filter (/=x) ns, es) (adj g x ++ xs)

dfs :: (Eq a) => Graph a -> Node a -> [Node a]
dfs g n = dfs' g [n]

bipartite :: (Ord a) => Graph a -> Bool
bipartite ([],_)    = True
bipartite g@(n:_,es) = all (uncurry (/=)) (map (\(v1,v2) -> (color v1, color v2)) nonTreeEdges)
  where
    tree         = dfs g n
    colored      = zip tree $ map (`mod`2) [0..]
    color v      = snd $ head $ dropWhile (\(x,_) -> x /= v) colored
    treeEdges    = zip tree (drop 1 tree)
    norm (a,b)   = if b < a then (b,a) else (a,b)
    nonTreeEdges = filter (`notElem` map norm treeEdges) $ map norm es

g1 :: Graph Int
g1 = ([1,2,3,4,5], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)])

main :: IO ()
main = print $ bipartite g1