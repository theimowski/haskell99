import Data.List (partition)

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

dfs :: (Eq a) => ([a], [(a,a)]) -> a -> [a]
dfs (vs,es) s = reverse $ fst $ dfs' (partition (==s) vs) s
  where
    Adj a = graphToAdj $ Graph vs es
    adj v = snd $ head $ dropWhile ((/=v) . fst) a
    dfs' xs v = 
      foldl 
        (\(vis,unvis) av -> 
          if av `elem` vis then 
            (vis,unvis) 
          else 
            dfs' (av:vis,filter (/=av) unvis) av) 
        xs 
        (adj v)

main :: IO ()
main = print $ dfs ([1,2,3,4,5,6,7], [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]) 1
