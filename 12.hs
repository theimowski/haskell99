data Encoded a = Multiple Int a | Single a deriving Show

decodeModified :: [Encoded a] -> [a]
decodeModified = 
  concat . (map (\x -> 
    case x of 
    Multiple m x -> replicate m x
    Single x -> [x]))
