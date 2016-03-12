prime_factors_mult :: Int -> [(Int,Int)]
prime_factors_mult x = prim x (2,0)
  where
    prim x (k,c)
      | x == 1 && 
        c /= 0         = [(k,c)]
      | x == 1         = []
      | x `mod` k == 0 = prim (x `div` k) (k,c+1)
      | x `mod` k /= 0 &&
        c /= 0         = (k,c) : prim x (k+1,0)
      | otherwise      = prim x (k+1,0)

phi :: Int -> Int
phi = foldl (\x (k,m) -> (k-1) * (k ^ (m-1)) * x) 1 . prime_factors_mult
