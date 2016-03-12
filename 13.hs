data Encoded a = Multiple Int a | Single a deriving Show

encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect (x:xs) =
  let (f,s) = span (==x) xs in
  case f of
  [] -> (Single x) : (encodeDirect s)
  ys -> (Multiple (1 + length ys) x) : (encodeDirect s)


