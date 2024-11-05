-- https://www.codewars.com/kata/53046ceefe87e4905e00072a/train/haskell

isPalindrome :: String -> Bool
isPalindrome =
  checkIsPalindrome . toLowerCase . handleBackslash . alphaNumericChars

handleBackslash :: [Char] -> [Char]
handleBackslash = show

checkIsPalindrome :: [Char] -> Bool
checkIsPalindrome (fstChar : sndChar : xs) =
  (fstChar == lstChar (sndChar : xs))
    && checkIsPalindrome (midChars (sndChar : xs))
 where
  lstChar  = last
  midChars = init
checkIsPalindrome xs = True

alphaNumericChars :: [Char] -> [Char]
alphaNumericChars = filter isAlphaNumericChar

isAlphaNumericChar :: Char -> Bool
isAlphaNumericChar x = isUpperChar || isLowerChar || isDigit
 where
  isUpperChar = asciiVal >= 65 && asciiVal <= 90
  isLowerChar = asciiVal >= 97 && asciiVal <= 122
  isDigit     = asciiVal >= 48 && asciiVal <= 57
  asciiVal    = fromEnum x

toLowerCase :: [Char] -> [Char]
toLowerCase [] = []
toLowerCase (x : xs) | isUpperChar = lowerChar : toLowerCase xs
                     | otherwise   = x : toLowerCase xs
 where
  lowerChar :: Char
  lowerChar = toEnum (asciiVal + 32)
  isUpperChar = asciiVal >= 65 && asciiVal <= 90
  asciiVal    = fromEnum x
