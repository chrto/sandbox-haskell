-- https://www.codewars.com/kata/5174a4c0f2769dd8b1000003/train/haskell

import Data.List (sort)

sortNumbers :: [Int] -> Maybe [Int]
sortNumbers xs
  | not (null xs) = Just (sort xs)
  | otherwise = Nothing
