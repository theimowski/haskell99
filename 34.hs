coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

totient :: Int -> Int
totient x = length $ filter (\y -> coprime x y) [1..x]
