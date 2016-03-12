mygcd :: Int -> Int -> Int
mygcd x 0 = x
mygcd x y = mygcd y (x `mod` y)
