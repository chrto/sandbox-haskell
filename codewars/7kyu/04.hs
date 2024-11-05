-- https://www.codewars.com/kata/54ff3102c1bad923760001f3/train/haskell

getCount :: String -> Int
getCount [] = 0
getCount (x : xs) | isVowel x = 1 + getCount xs
                  | otherwise = getCount xs
 where
  isVowel :: Char -> Bool
  isVowel = isIn "aeiou"
   where
    isIn :: [Char] -> Char -> Bool
    isIn [] _ = False
    isIn (y : ys) x | x == y    = True
                    | otherwise = isIn ys x
