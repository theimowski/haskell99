pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (f,s) = span (==x) xs in ((x:f):pack s)

collect :: (Eq a) => [[a]] -> [(Int,a)]
collect [] = []
collect (x:xs) = (length (x), head x) : collect xs

encode :: (Eq a) => [a] -> [(Int,a)]
encode = collect . pack
