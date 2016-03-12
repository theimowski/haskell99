combinations :: Int -> [a] -> [[a]]
combinations n xs = combi n xs []
  where
    combi 0 _ acc = [reverse acc]
    combi _ [] _ = []
    combi n (x:xs) acc = combi (n-1) xs (x:acc) `mappend` combi n xs acc
