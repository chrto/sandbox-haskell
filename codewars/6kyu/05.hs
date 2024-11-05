-- https://www.codewars.com/kata/52105fab0bd0ce9dd00000fe/train/haskell

-- QuickSort algorithm
sort :: Ord a => [a] -> [a]
sort []       = []
sort (x : xs) = sort lt ++ [x] ++ sort gt
 where
  lt = [ y | y <- xs, y <= x ]
  gt = [ y | y <- xs, y > x ]

-- Insertion Sort algorithm
-- same as:
-- sort :: Ord a => [a] -> [a]
-- sort = foldr insertIntoSorted []

sort' :: Ord a => [a] -> [a]
sort' []       = []
sort' (x : xs) = insertIntoSorted x $ sort' xs

insertIntoSorted :: (Ord a) => a -> [a] -> [a]
insertIntoSorted y [] = y : []
insertIntoSorted y (x : xs) | y <= x    = y : x : xs
                            | otherwise = x : insertIntoSorted y xs
