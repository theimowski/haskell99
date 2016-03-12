pack :: (Eq a) => [a] -> [[a]]
pack xs =
  let (ys,y) = foldr (\next (acc,cur) -> case cur of 
        [] -> (acc,[next])
        (y:_) ->
          if y == next then
            (acc,(next:cur))
          else
            ((cur:acc),[next]))  ([],[]) xs
  in (y:ys)

