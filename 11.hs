pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (f,s) = span (==x) xs in ((x:f):pack s)

collect :: (Eq a) => [[a]] -> [(Int,a)]
collect [] = []
collect (x:xs) = (length (x), head x) : collect xs

encode :: (Eq a) => [a] -> [(Int,a)]
encode = collect . pack

data Encoded a = Multiple Int a | Single a deriving Show

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = map (\(c,x) ->
                       case (c,x) of
                       (1,x) -> Single x
                       (m,x) -> Multiple m x) . encode
