import Data.List

data Graph a = Graph [a] [(a,a)] deriving (Show,Eq)

k3 :: Graph Int
k3 = Graph [0,1,2] [(0,1),(0,2),(1,2)]

k4 :: Graph Int
k4 = Graph [0,1,2,3] [(0,1),(0,2),(0,3),(1,2),(1,3),(2,3)]

k5 :: Graph Int
k5 = Graph [0,1,2,3,4] [(0,1),(0,2),(0,3),(0,4),(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

pairToList :: (a,a) -> [a]
pairToList (x,y) = [x,y]

spanStep :: Ord a => [(a,a)] -> Graph a -> [Graph a]
spanStep es g@(Graph vs ges)
  | length ges == length vs - 1 = [g]
  | otherwise =
    filter (\e -> (<2) $ length $ intersect (pairToList e) (nub $ ges >>= pairToList)) es
    >>= (\e -> spanStep (filter (/=e) es) (Graph vs (e:ges)))

spanTrees :: Ord a => Graph a -> [Graph a]
spanTrees (Graph vs es) = 
  nub $
  map (\(Graph v e) -> Graph v (sort e)) $
  spanStep es (Graph vs [])

main :: IO ()
main = print $ length $ spanTrees k5