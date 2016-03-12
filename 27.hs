import qualified Data.List as L

combinations :: Int -> [a] -> [[a]]
combinations n xs = combi n xs []
  where
    combi 0 _ acc = [reverse acc]
    combi _ [] _ = []
    combi n (x:xs) acc = combi (n-1) xs (x:acc) `mappend` combi n xs acc

group :: (Eq a) => [Int] -> [a] -> [[[a]]]
group ns xs = group' ns xs []
  where 
    group' [] _ acc = [reverse acc]
    group' _ [] _ = []
    group' (n:ns) xs acc = concatMap (\x -> group' ns (xs L.\\ x) (x:acc)) (combinations n xs)
