-- https://www.codewars.com/kata/523f5d21c841566fde000009/train/haskell

-- difference :: Eq a => [a] -> [a] -> [a]
-- difference xs [] = xs
-- difference xs (y : ys) = difference (removeFromList y xs) ys
difference :: Eq a => [a] -> [a] -> [a]
difference = foldr removeFromList
 where
  removeFromList :: Eq a => a -> [a] -> [a]
  removeFromList y xs = [ x | x <- xs, x /= y ]

difference' :: Eq a => [a] -> [a] -> [a]
difference' = foldl removeFormList
 where
  removeFormList :: Eq a => [a] -> a -> [a]
  removeFormList [] _ = []
  removeFormList (x : xs) y | x == y    = removeFormList xs y
                            | otherwise = x : removeFormList xs y
difference'' :: Eq a => [a] -> [a] -> [a]
difference'' xs []       = xs
difference'' xs (y : ys) = difference'' (removeFormList xs y) ys
 where
  removeFormList :: Eq a => [a] -> a -> [a]
  removeFormList [] _ = []
  removeFormList (x : xs) y | x == y    = removeFormList xs y
                            | otherwise = x : removeFormList xs y

