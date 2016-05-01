import Data.List (minimumBy,nub,intersect,sort)

prim :: Ord a => [a] -> [(a,a,Int)] -> [(a,a,Int)]
prim vs es = sort $ prim' [] es
  where
    prim' acc left
      | length acc == length vs - 1 = acc
      | otherwise  =
        let e = minimumBy thrd $ filter (tree acc) left in
        prim' (e:acc) (filter (/=e) left)
    thrd (_,_,x) (_,_,y) = compare x y
    tree acc e =
      length (vertices e `intersect` nub (acc >>= vertices)) < 2
    vertices (x,y,_) = [x,y]

main :: IO ()
main = print $ prim [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]