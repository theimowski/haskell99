perm :: Int -> [[Int]]
perm 0 = [[]]
perm n = [take (x-1) p ++ [n] ++ drop (x-1) p | p <- perm (n-1) ,x <- [1..n]]

queens :: Int -> [[Int]]
queens n = filter (q . rowcol) (perm n)
  where
    rowcol = zip [1..8]
    q xs   = all (\x -> diag x $ filter (/=x) xs) xs
    diag x = not . any (d x)
    d x y  = abs (fst x - fst y) == abs (snd x - snd y)

main :: IO ()
    
main = print $ length $ queens 8