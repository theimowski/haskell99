import Data.List

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show

constructTree :: (Ord w, Num w) => [(w, HTree a)] -> HTree a
constructTree [(_,t)] = t
constructTree ((wa,xa):(wb,xb):xs) = 
  constructTree $ 
    insertBy (\(w1,_) (w2,_) -> compare w1 w2) (wa + wb, Branch xa xb) xs

compress :: (Ord a) => HTree a -> [(a,String)]
compress t = compress' t ""
  where 
    compress' (Leaf a) acc = [(a,acc)]
    compress' (Branch a b) acc = 
      compress' a (acc ++ "0") ++
      compress' b (acc ++ "1")

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,String)]
huffman xs =
  sortBy (\(a1,_) (a2,_) -> compare a1 a2) $
  compress $
  constructTree $
  map (\(a,w) -> (w, Leaf a)) $
  sortBy (\(_,w1) (_,w2) -> compare w1 w2) xs
