mySet :: [(Float, Float)]
mySet = [(1, 2), (2, 3), (3, 4), (4, 5)]

lagrange :: [(Float, Float)] -> Float -> Float
lagrange set x = foldl (\acc (xj, yj) -> acc + (yj * lj xj)) 0 set
 where
  lj xj = foldl
    (\acc (xm, _) -> if xm == xj then acc else (x - xm) / (xj - xm) * acc)
    1
    set


lagrange' :: [(Float, Float)] -> Float -> Float
lagrange' set x = foldr fr 0 set
 where
  fr :: (Float, Float) -> Float -> Float
  fr (xj, yj) acc = (l xj * yj) + acc
   where
    l :: Float -> Float
    l xj = foldr fr' 1 set
     where
      fr' :: (Float, Float) -> Float -> Float
      fr' (xm, _) acc | xm == xj  = acc
                      | otherwise = (x - xm) / (xj - xm) * acc

