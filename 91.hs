import Data.List
import Data.Ord

sortByF :: (Ord b) => (a -> b) -> [a] -> [a]
sortByF = sortBy . comparing

jumps :: Int -> (Int,Int) -> [(Int,Int)]
jumps n from = 
  [(x,y) | 
   (x',y') <- [(-1,-2),(-1,2),(1,-2),(1,2),(-2,-1),(-2,1),(2,-1),(2,1)],
   let x = fst from + x',
   let y = snd from + y',
   x > 0, y > 0, x <= n, y <= n]

knightsTo :: Int -> (Int,Int) -> [[(Int,Int)]]
knightsTo n end = knights [end] squares
  where
    squares             = [(x,y) | x <- [1..n], y <- [1..n], (x,y) /= end ]
    knights acc []      = [acc]
    knights (a:cc) rest = 
      sortByF (length . intersect rest . jumps n)
      rest `intersect` jumps n a
      >>= (\j -> knights (j:a:cc) (filter (/=j) rest))

closedKnights :: Int -> [[(Int,Int)]]
closedKnights n = closed [end] squares
  where
    start                = (1,1)
    end                  = (2,3)
    squares              = [(x,y) | x <- [1..n], y <- [1..n], (x,y) /= end ]
    f rest x
      | x == start = 10
      | otherwise  = length $ intersect rest $ jumps n x
    closed (acc) []      = [acc]
    closed (a:cc) rest   =
      sortByF (f rest)
      rest `intersect` jumps n a
      >>= (\j -> closed (j:a:cc) (filter (/=j) rest))

main :: IO ()
main = print $ head $ closedKnights 8