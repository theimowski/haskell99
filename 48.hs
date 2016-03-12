and'  :: Bool -> Bool -> Bool
or'   :: Bool -> Bool -> Bool
nand' :: Bool -> Bool -> Bool
nor'  :: Bool -> Bool -> Bool
xor'  :: Bool -> Bool -> Bool
impl' :: Bool -> Bool -> Bool
equ'  :: Bool -> Bool -> Bool

and'  x y = if x then y       else False
or'   x y = if x then True    else y
nand' x y = if x then (not y) else True
nor'  x y = if x then False   else (not y)
xor'  x y = if x then (not y) else y
impl' x y = if x then y       else True
equ'  x y = x == y


infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 7 `equ'`

table :: (Bool -> Bool -> Bool) -> IO ()
table f = 
  mapM_
    putStrLn 
    [ show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True,False], y <- [True,False]]

perm :: Int -> [a] -> [[a]]
perm 0 _ = [[]]
perm n xs = perm (n-1) xs >>= \x -> map (:x) xs

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f =
  mapM_
    putStrLn
    (map  (\xs -> show (xs ++ [f xs])) $ map reverse $ perm n [True, False])

