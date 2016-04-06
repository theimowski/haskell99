data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree :: Tree Char
tree = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

ds2Tree :: String -> Tree Char
ds2Tree = fst . ds2Tree'
  where
    ds2Tree' ('.' : ds) = (Empty, ds)
    ds2Tree' ( d  : ds) = (Branch d l r, ds'')
      where
        (l, ds')  = ds2Tree' ds
        (r, ds'') = ds2Tree' ds'

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch a l r) = a : tree2ds l ++ tree2ds r

main :: IO ()
main = print (tree == ds2Tree (tree2ds tree))