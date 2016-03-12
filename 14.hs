dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
