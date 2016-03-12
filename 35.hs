import Data.List
import Data.Maybe

isPrime :: Int -> Bool
isPrime x = (length $ filter (\y -> x `mod` y == 0) [2..(x `div` 2)]) == 0

prim :: Int -> [Int] -> [Int]

prim x acc
  | isPrime x = reverse (x:acc)
  | otherwise =
    let next = fromJust $ find (\y -> isPrime y && x `mod` y == 0)  [2..(floor $ sqrt $ fromIntegral x)] in
    prim (x `div` next) (next:acc)

primeFactors :: Int -> [Int]
primeFactors x = prim x []


primeFactors' :: Int -> [Int]
primeFactors' x = prim' x 2
  where
    prim' x f 
      | f*f > x        = [x]
      | x `mod` f == 0 = f : prim' (x `div` f) f
      | otherwise      = prim' x (f + 1)
