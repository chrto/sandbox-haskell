-- https://www.codewars.com/kata/526571aae218b8ee490006f4/train/haskell

import Data.List (unfoldr)

countBits :: Int -> Int
countBits = sum . unfoldr (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))

-- countBits :: Int -> Int
-- countBits = length . filter (== 1) . unfoldr (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))
