paths :: Eq a => [a] -> a -> a -> [(a,a)] -> [[a]]
paths acc s d es
  | s == d    = [reverse (d:acc)]
  | otherwise =
    [ p | edge@(f,sc) <- es,
          f == s,
          p <- paths (f:acc) sc d (filter (/=edge) es) ]

cycles :: Eq a => a -> [(a,a)] -> [[a]]
cycles n es = 
  [ n:p | edge@(f,s) <- es, 
          f == n,
          p <- paths [] s n (filter (/=edge) es) ]

main :: IO ()
main = print $ cycles 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]