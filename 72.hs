data Tree a = Node a [Tree a] deriving (Show,Eq)

tree5 :: Tree Char
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

bottomUp :: Tree a -> [a]
bottomUp (Node x xs) = concatMap bottomUp xs ++ [x]

main :: IO ()
main = print $ bottomUp tree5