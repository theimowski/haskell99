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

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

ldepth :: Tree a -> Int
ldepth Empty = 0
ldepth (Branch _ l _) = 1 + ldepth l

layout :: Tree a -> Tree (a, (Int, Int))
layout t = layout' t (2^(d - 1)) 1
  where
    d      = depth t
    ld     = ldepth t
    offset = (2 ^ (d - ld)) - 1
    layout' Empty _ _          = Empty
    layout' (Branch a l r) x y = Branch (a,(x - offset,y)) l' r'
      where
        b  = 2 ^ (d - y - 1)
        l' = layout' l (x-b) (y+1)
        r' = layout' r (x+b) (y+1)


main :: IO()
main = print $ layout tree65