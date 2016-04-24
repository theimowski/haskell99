paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths s e xs = 
  map (\(x,y) -> [y,x]) (filter ((==s) . fst) xs) >>= path
  where
    path l@(p:ps)
      | p == e    = [reverse l]
      | otherwise = 
        map ((:l) . snd) (filter (\(x,y) -> x == p && y `notElem` ps) xs) >>= path

main :: IO ()
main = print $ paths 1 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]