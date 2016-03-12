repli :: [a] -> Int -> [a]
repli xs x = concatMap (replicate x) xs
