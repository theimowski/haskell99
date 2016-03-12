split :: [a] -> Int -> ([a],[a])
split xs n =
  split' ([],xs) n 
  where
    split' (bef,aft) 0 = (reverse bef, aft)
    split' (bef,(x:aft)) k = split' (x:bef,aft) (k-1)
