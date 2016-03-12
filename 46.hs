and'  :: Bool -> Bool -> Bool
or'   :: Bool -> Bool -> Bool
nand' :: Bool -> Bool -> Bool
nor'  :: Bool -> Bool -> Bool
xor'  :: Bool -> Bool -> Bool
impl' :: Bool -> Bool -> Bool

and'  x y = if x then y       else False
or'   x y = if x then True    else y
nand' x y = if x then (not y) else True
nor'  x y = if x then False   else (not y)
xor'  x y = if x then (not y) else y
impl' x y = if x then y       else True

table :: (Bool -> Bool -> Bool) -> IO ()
table f = 
  mapM_
    putStrLn 
    [ show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True,False], y <- [True,False]] 
