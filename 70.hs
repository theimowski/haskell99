
data Tree a = Node a [Tree a] deriving (Show,Eq)

tree5 :: Tree Char
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

nnodes :: Tree a -> Int
nnodes (Node _ xs) = 1 + sum (map nnodes xs)

treeToString :: Tree Char -> String
treeToString (Node x xs) = x : concatMap treeToString xs ++ "^"

subtrees :: String -> String -> Int -> [Tree Char]
subtrees [] [] 0            = []
subtrees acc ('^':xs) 1     = stringToTree (reverse ('^':acc)) : subtrees [] xs 0
subtrees acc ('^':xs) depth = subtrees ('^':acc) xs (depth - 1)
subtrees acc (x:xs)   depth = subtrees (x:acc) xs (depth + 1)

stringToTree :: String -> Tree Char
stringToTree (c:cs) = Node c (subtrees [] (init cs) 0)

main :: IO ()
main = print $ stringToTree "afg^^c^bd^e^^^" == tree5