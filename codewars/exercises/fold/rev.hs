rev :: [a] -> [a]
rev = foldl f' []
 where
  f  = \acc x -> x : acc
  f' = flip (:)

rev' :: [a] -> [a]
rev' = foldr f [] where f = \x acc -> acc ++ [x]
