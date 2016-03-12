data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

leaf a = Branch a Empty Empty

hbalTree :: a -> Integer -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [leaf x]
hbalTree x n =
  [ t | a <- hbalTree x (n-1),
        b <- hbalTree x (n-2),
        t <- [ Branch x a a, 
               Branch x a b,
               Branch x b a ] ]

main :: IO ()
main = putStrLn $ show $ take 4 $ hbalTree 'x' 3