isPrime x = (length $ filter (\y -> x `mod` y == 0) [2..(x `div` 2)]) == 0

primesR :: Int -> Int -> [Int]
primesR f t = [x | x <- [f..t], isPrime x]
