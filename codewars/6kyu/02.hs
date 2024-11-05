-- https://www.codewars.com/kata/52b757663a95b11b3d00062d/train/haskell

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
toWeirdCase :: String -> String
toWeirdCase = transform 0

transform :: Int -> String -> String
transform _ [] = []
transform i (x : xs) | x == ' '  = x : transform 0 xs
                     | even i    = toUpper x : transform (i + 1) xs
                     | otherwise = toLower x : transform (i + 1) xs
