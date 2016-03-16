data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

minNodesSeq :: [Int]
minNodesSeq = 0:1:zipWith ((+).(+1)) minNodesSeq (tail minNodesSeq)

minNodes :: Int -> Int
minNodes h = minNodesSeq !! h

maxNodes :: Int -> Int
maxNodes h = 2^h - 1

minHeight :: Int -> Int
minHeight n = ceiling $ logBase (2::Double) $ fromIntegral (n+1)

maxHeight :: Int -> Int
maxHeight n = length (takeWhile (n>=) minNodesSeq) - 1

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes a n = concatMap (hbalTrees n) [minHeight n .. maxHeight n]
  where
    hbalTrees _  0 = [Empty]
    hbalTrees _  1 = [Branch a Empty Empty]
    hbalTrees ns h =
      [ Branch a l r |
        (hl,hr) <- [(h-1,h-2),(h-1,h-1),(h-2,h-1)],
        let min_nl = max (minNodes hl) (ns - 1 - maxNodes hr),
        let max_nl = min (maxNodes hl) (ns - 1 - minNodes hr),
        nl <- [min_nl .. max_nl],
        let nr = ns - nl - 1,
        l <- hbalTrees nl hl,
        r <- hbalTrees nr hr ]

main :: IO()
main = print $ length $ hbalTreeNodes 'x' 15