import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

--leaf a = Branch a Empty Empty

maxNodes h = floor $ 2 ** h - 1

minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + (minNodes (h - 1) + minNodes (h - 2))

maxHeight n = fst $ fromJust $ find (\(h,ns) -> n >= ns) $ map (\h -> (h, minNodes h)) [0..]

main = print (map (maxHeight) [0..10])
--main = print (map maxHeight [0..10])