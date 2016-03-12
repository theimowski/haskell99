isPrime :: Int -> Bool
isPrime x = (length $ filter (\y -> x `mod` y == 0) [2..(x `div` 2)]) == 0
