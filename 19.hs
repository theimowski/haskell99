rotate :: [a] -> Int -> [a]
rotate xs k =
  let l = length xs in
  let n = k `mod` l in
  let s = if n < 0 then l + n else n in
  (drop s xs) ++ (take s xs)
