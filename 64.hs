data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show,Eq)

tree64 :: Tree Char
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

layout :: Tree a -> Tree (a, (Int, Int))
layout t = fst $ layout' t 1 1
  where
    layout' Empty x _          = (Empty, x)
    layout' (Branch a l r) x y = (Branch (a,(x',y)) l' r', x'')
      where 
        (l',x')  = layout' l x (y + 1)
        (r',x'') = layout' r (x' + 1) (y + 1)


main :: IO ()
main = print $ layout tree64