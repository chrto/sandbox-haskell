-- https://www.codewars.com/kata/58f8a3a27a5c28d92e000144/train/haskell

import Data.Maybe (listToMaybe)

firstNonConsecutive :: (Eq a, Enum a) => [a] -> Maybe a
firstNonConsecutive xs = (\(first : second : rest) -> if succ first == second then firstNonConsecutive (second : rest) else Just second) =<< (if length xs > 1 then Just xs else Nothing)

firstNonConsecutive' :: (Eq a, Enum a) => [a] -> Maybe a
firstNonConsecutive' xs = listToMaybe [b | (a, b) <- zip xs (tail xs), succ a /= b]

firstNonConsecutive'' :: (Eq a, Enum a) => [a] -> Maybe a
firstNonConsecutive'' xs = listToMaybe . map snd . filter (\(a, b) -> succ a /= b) $ zip xs (tail xs)