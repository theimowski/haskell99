data Tree a = Node a [Tree a] deriving (Show,Eq)

tree5 :: Tree Char
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

lisp :: Tree Char -> String
lisp (Node a []) = [a]
lisp (Node a ts) = 
  "(" ++ [a] ++ concatMap ((" "++) . lisp) ts ++ ")"

subtrees :: String -> String -> Int -> [Tree Char]
subtrees [] [] 0            = []
subtrees acc [] 0           = [stringToTree (reverse acc)]
subtrees acc (' ':xs) 0     = subtrees acc xs 0
subtrees acc ('(':xs) depth = subtrees ('(':acc) xs (depth + 1)
subtrees acc (')':xs) 1     = stringToTree (reverse (')':acc)) : subtrees [] xs 0
subtrees acc (')':xs) depth = subtrees (')':acc) xs (depth - 1)
subtrees acc (x:xs)   0     = stringToTree [x] : subtrees [] xs 0
subtrees acc (x:xs)   depth = subtrees (x:acc) xs depth

stringToTree :: String -> Tree Char
stringToTree ('(':c:cs) = Node c (subtrees [] (init cs) 0)
stringToTree [c]        = Node c []

main :: IO ()
main = print $ stringToTree "(a (f g) c (b d e))"