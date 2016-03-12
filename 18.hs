slice :: [a] -> Int -> Int -> [a]
slice xs s e = take (e-s+1) $ drop (s-1) xs

-- flip drop
