import Data.List

range :: (Enum a, Eq a) => a -> a -> [a]
range s e = 
  unfoldr (\x -> 
    if pred x == e then
      Nothing 
    else
      Just (x, succ x)) s
