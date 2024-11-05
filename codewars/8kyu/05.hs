-- https://www.codewars.com/kata/576bb71bbbcf0951d5000044/train/haskell

countPositivesSumNegatives :: Maybe [Int] -> [Int]
countPositivesSumNegatives Nothing   = []
countPositivesSumNegatives (Just []) = []
countPositivesSumNegatives (Just xs) = [length greater, sum lesser]
 where
  lesser  = [ x | x <- xs, x < 0 ]
  greater = [ x | x <- xs, x > 0 ]

countPositivesSumNegatives'' :: Maybe [Int] -> [Int]
countPositivesSumNegatives'' Nothing   = []
countPositivesSumNegatives'' (Just []) = []
countPositivesSumNegatives'' (Just (x : xs)) =
  op x $ countPositivesSumNegatives'' (Just xs)


countPositivesSumNegatives' :: Maybe [Int] -> [Int]
countPositivesSumNegatives' Nothing   = []
countPositivesSumNegatives' (Just []) = []
countPositivesSumNegatives' (Just xs) = foldr op [] xs

op :: Int -> [Int] -> [Int]
op x [] | x < 0     = [0, x]
        | x > 0     = [1, 0]
        | otherwise = []
op x [c, s] | x < 0     = [c, s + x]
            | x > 0     = [c + 1, s]
            | otherwise = [c, s]
