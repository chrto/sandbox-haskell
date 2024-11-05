-- https://www.codewars.com/kata/5656b6906de340bd1b0000ac/train/haskell

import Data.List (nub, sort)

longest :: [Char] -> [Char] -> [Char]
longest =
  (sort .)
    . (nub .)
    . (++)

longest' :: [Char] -> [Char] -> [Char]
longest' s1 s2 =
  sort $
    nub $
      s1 ++ s2