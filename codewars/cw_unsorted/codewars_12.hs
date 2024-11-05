-- https://www.codewars.com/kata/609eee71109f860006c377d1

import Data.List (splitAt)

lastSurvivor :: String -> [Int] -> Char
lastSurvivor str [] = head str
lastSurvivor str (n : ns) = lastSurvivor str' ns
  where
    str' = (\(lstr, _ : rstr) -> (++) lstr rstr) $ splitAt n str

lastSurvivor' :: String -> [Int] -> Char
lastSurvivor' str xs
  | null xs = head str
  | otherwise = lastSurvivor str' $ tail xs
  where
    str' = (\(lxs, _ : rxs) -> (++) lxs rxs) $ splitAt (head xs) str
