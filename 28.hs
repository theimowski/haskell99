import Data.List

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort [x] = [x]
lsort (x:xs) = lsort smaller ++ [x] ++ lsort larger
  where 
    (smaller,larger) = partition (\y -> length y < length x) xs

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort [x] = [x]
lfsort (x:xs) = lfsort smaller ++ [x] ++ lfsort larger
  where
    lengthFreq y = length $ filter (\z -> length z == length y) (x:xs)
    (smaller,larger) = partition (\y -> lengthFreq y < lengthFreq x) xs
