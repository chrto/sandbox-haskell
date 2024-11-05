-- https://www.codewars.com/kata/52fba66badcd10859f00097e/train/haskell
import           Data.Char                      ( chr
                                                , ord
                                                )
disemvowel :: String -> String
disemvowel = filter isNotVowel
 where
  isNotVowel :: Char -> Bool
  isNotVowel = not . isIn vowels

  isIn :: [Char] -> Char -> Bool
  isIn []       _ = False
  isIn (x : xs) y = (y == x) || isIn xs y

  vowels :: [Char]
  vowels = "AEIOU" >>= (\x -> [x, toLowerCase x])

  toLowerCase :: Char -> Char
  toLowerCase c = chr $ (+) 32 (ord c)
