-- https://www.codewars.com/kata/57a2013acf1fa5bfc4000921/train/haskell

avg :: [Float] -> Float
avg [] = 0
avg l  = sum' l / fromIntegral (length' l)
 where
  sum' :: [Float] -> Float
  sum' []       = 0
  sum' (x : xs) = x + sum' xs

  length' :: [Float] -> Int
  length' []       = 0
  length' (x : xs) = 1 + length' xs


