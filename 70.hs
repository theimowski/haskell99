
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

--s2t :: String -> (Tree Char, String)
--s2t (x:xs) = Node x (fst $ unfoldr unfolder xs)
--
--unfolder :: String -> Maybe ((Tree Char, String), String)
--unfolder xs =
--  case xs of
--    ['^']  -> Nothing
--    (y:ys) -> Just ()
--
--stringToTree :: String -> Tree Char
--stringToTree = fst . s2t
--  where
--    unfolder xs =
--      case xs of
--        ['^']  -> Nothing
--        (x:xs) -> Just (s2t (x:xs))
--    s2t (x:xs) = Node x (unfoldr unfolder xs)

-- unfolder :: String -> Maybe ((Tree Char, String), String)
-- unfolder ('^':_) = Nothing
-- unfolder (x:xs)  = let (t,r) = s2t (x:xs) in Just ((t,r), r)
-- 
-- s2t :: String -> (Tree Char, String)
-- s2t (x:xs) = (Node x ys, rest)
--   where
--     (ys, rests) = unzip $ unfoldr unfolder xs
--     rest = case rests of [] -> xs
--                          r  -> last r
-- 
-- folder :: Char -> ([Tree Char], String) -> ([Tree Char], String)
-- folder '^' (ts, xs) = (ts, xs)
-- folder c (xs,cs)      = let (t,rest) = s2t2 cs in (t:xs,rest)
-- 
-- s2t2 :: String -> (Tree Char, String)
-- s2t2 (x:xs) = (Node x ys, rest)
--   where
--     (ys, rest) = foldr folder ([],xs) xs

subtrees :: String -> String -> Int -> [String]
subtrees [] [] 0            = []
subtrees acc ('^':xs) 1     = reverse ('^':acc) : subtrees [] xs 0
subtrees acc ('^':xs) depth = subtrees ('^':acc) xs (depth - 1)
subtrees acc (x:xs)   depth = subtrees (x:acc) xs (depth + 1)

s2t :: String -> Tree Char
s2t (c:cs) = Node c ds
  where
    ds = map s2t (subtrees [] (init cs) 0)

main :: IO ()
main = print $ s2t "afg^^c^bd^e^^^" == tree5