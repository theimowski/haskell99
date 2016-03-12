import Data.List

isPrime :: Int -> Bool
isPrime x = (length $ filter (\y -> x `mod` y == 0) [2..(x `div` 2)]) == 0

goldbach :: Int -> Maybe (Int, Int)
goldbach x
  | even x = fmap (\y -> (y,x-y)) $ find (\y -> isPrime y && isPrime (x-y)) [2..(x `div` 2)]
  | otherwise = Nothing
  
