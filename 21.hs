insertAt :: a -> [a] -> Int -> [a]
insertAt x xs 1 = x:xs
insertAt x (y:xs) n = y : (insertAt x xs (n-1))
