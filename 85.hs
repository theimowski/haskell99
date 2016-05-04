data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adj   a = Adj   [(a,[a])]   deriving (Show, Eq)

graphToAdj :: Eq a => Graph a -> Adj a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
  where
    f (y1,y2)
      | y1 == x   = [y2]
      | y2 == x   = [y1]
      | otherwise = []
    Adj zs = graphToAdj (Graph xs ys)

canon :: Eq a => Graph a -> String
canon g@(Graph xs ys) = minimum $ map (concat . f) perm
  where
    Adj a= graphToAdj g
    n    = length a
    perm = 
      foldr 
        (\_ ts -> [ h : t | t <- ts, h <- [1..n], h `notElem` t])
        [[]]
        [1..n]
    f p  = map (\i -> concatMap (show . m p) $ snd (a !! (i-1))) p
    m p i= snd $ head $ dropWhile ((/=i) . fst) $ zip xs p

iso :: Eq a => Graph a -> Graph a -> Bool
iso g@(Graph xs ys) g'@(Graph xs' ys') =
  length xs == length xs' &&
  length ys == length ys' &&
  canon g   == canon g'

graphG1 :: Graph Int
graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
      [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8),
       (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
 
graphH1 :: Graph Int
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
      [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7),
       (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]

main :: IO ()
--main = print $ iso graphG1 graphH1
main = print $ map canon [graphG1,graphH1]