removeAt :: Int -> [a] -> (a,[a])
removeAt i xs = remove i [] xs
  where
    remove 1 bef (x:aft) = (x, reverse bef ++ aft)
    remove k bef (x:aft) = remove (k-1) (x:bef) aft
