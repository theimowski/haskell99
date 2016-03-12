{-# OPTIONS_GHC -O2 -fno-cse #-}
import Data.List
-- tree-merging Eratosthenes sieve
--  producing infinite list of all prime numbers
primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

primesR :: Int -> Int -> [Int]
primesR s e = takeWhile (<=e) $ dropWhile (<s) $ primesTME

isPrime n | n < 4 = n /= 1 
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n

goldbach :: Int -> (Int, Int)
goldbach x = head [(y,x-y) | y <- primesR 2 (x `div` 2), isPrime (x-y)]

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList s e = map goldbach (filter even [s..e])

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' s e t = filter (\(x,y) -> (x > t) && (y > t)) (goldbachList s e)
