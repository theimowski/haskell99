import Data.List
import Data.Tuple.Extra

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree :: Tree Char
tree = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

treeToString :: Tree Char -> String
treeToString Empty                  = ""
treeToString (Branch a Empty Empty) = [a]
treeToString (Branch a l r)         =
  a : "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

index :: String -> Int -> Int -> Int
index (',':_)  0 x = x
index ('(':xs) p x = index xs (p+1) (x+1)
index (')':xs) p x = index xs (p-1) (x+1)
index (_:xs)   p x = index xs p     (x+1)

stringToTree :: String -> Tree Char
stringToTree []       = Empty
stringToTree [a]      = Branch a Empty Empty
stringToTree (a : xs) = Branch a l' r'
  where
    xs' = init $ tail xs 
    i = index xs' 0 0
    (l,r) = splitAt i xs'
    l' = stringToTree l
    r' = stringToTree (tail r)

main :: IO ()
main = print $ (((==) tree) . stringToTree . treeToString) tree