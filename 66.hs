data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree65 :: Tree Char
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

tree65' :: Tree (Char, Int)
tree65' = Branch ('n', 2)
                (Branch ('k', 1)
                        (Branch ('c', 1)
                                (Branch ('a', 0) Empty Empty)
                                (Branch ('e', 1)
                                        (Branch ('d', 0) Empty Empty)
                                        (Branch ('g', 0) Empty Empty)
                                )
                        )
                        (Branch ('m', 0) Empty Empty)
                )
                (Branch ('u', 1)
                        (Branch ('p', 1)
                                Empty
                                (Branch ('q', 0) Empty Empty)
                        )
                        Empty
                )

distances :: Tree a -> Tree (a, Int)
distances t = fst $ distances' t
  where
    distances' Empty                  = (Empty, ([],[]))
    distances' (Branch a Empty Empty) = (Branch (a, 0) Empty Empty, ([0],[0]))
    distances' (Branch a l r)         = (Branch (a, d) l' r', (dl', dr'))
      where
        (l', (ldl,ldr)) = distances' l
        (r', (rdl,rdr)) = distances' r
        check i         = and $ zipWith (\ld rd -> ld < rd) (map (\e -> e - i) ldr) (map (+i) rdl)
        d               = head $ dropWhile (not . check) [1..]
        (mins,maxs)     = unzip $ zipWith (\ld rd -> (min ld rd,max ld rd)) ldl rdr
        dl'             = -d:mins
        dr'             = d:maxs

layout :: Tree a -> Tree (a, (Int, Int))
layout t = layout' t' 1 1
  where
    t'                              = distances t
    offset Empty                    = 0
    offset (Branch (_, b) l _)      = b + offset l
    o                               = offset t'
    layout' Empty _ _               = Empty
    layout' (Branch (a, b) l r) x y = Branch (a, (x + o, y)) l' r'
      where
        l' = layout' l (x - b) (y+1)
        r' = layout' r (x + b) (y+1)

main :: IO()
main = print $ layout tree65