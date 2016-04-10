data Tree a = Node a [Tree a] deriving (Show,Eq)

tree5 :: Tree Char
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

tree2 :: Tree Char
tree2 = Node 'a' [Node 'b' [Node 'c' []]]

ipl :: Tree a -> Int
ipl = ipl' 1
  where
    ipl' depth (Node _ xs) = sum $ map ((+depth) . ipl' (depth+1)) xs

main :: IO ()
main = print $ ipl tree2