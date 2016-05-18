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

connectedcomponents :: (Eq a) => Graph a -> [[Node a]]
connectedcomponents ([],_)      = []
connectedcomponents g@(n:ns,es) = component : connectedcomponents (ns',es)
  where
    component = dfs g n
    ns'       = filter (`notElem` component) (n:ns)

g1 :: Graph Int
g1 = ([1,2,3,4,5,6,7,8,9,10], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7),(8,10)])

main :: IO () 
main = print $ connectedcomponents g1