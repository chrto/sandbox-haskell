-- https://www.codewars.com/kata/57eae20f5500ad98e50002c5/train/haskell
noSpace :: String -> String
noSpace xs = [ x | x <- xs, x /= ' ' ]

noSpace'' :: String -> String
noSpace'' [] = []
noSpace'' (x : xs) | x == ' '  = noSpace'' xs
                   | otherwise = x : noSpace'' xs

noSpace' :: String -> String
noSpace' = filter (/= ' ')
