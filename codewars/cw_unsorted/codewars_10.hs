-- https://www.codewars.com/kata/56676e8fabd2d1ff3000000c/haskell

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Printf (printf)

findNeedle :: [String] -> String
findNeedle s = case show <$> elemIndex "needle" s of
  Just index -> "found the needle at position " ++ index
  Nothing -> ""

findNeedle' :: [String] -> String
findNeedle' s = case elemIndex "needle" s of
  Just index -> printf "found the needle at position %d" index
  Nothing -> ""

findNeedle'' :: [String] -> String
findNeedle'' = printf "found the needle at position %d" . fromJust . elemIndex "needle"