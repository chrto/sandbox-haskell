-- https://www.codewars.com/kata/5552101f47fc5178b1000050/train/haskell

import Data.Char (digitToInt)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

digpow :: Integer -> Integer -> Integer
digpow n p = integerToString n
               |> powNumber p
               |> getResult n
 where
  powNumber :: Integer -> [Char] -> Integer
  powNumber p []       = 0
  powNumber p (n : ns) = charToInteger n ^ p + powNumber (p + 1) ns

  integerToString :: Integer -> [Char]
  integerToString = show

  charToInteger :: Char -> Integer
  charToInteger = toInteger .digitToInt

  getResult :: Integer -> Integer -> Integer
  getResult x y | mod y x == 0 = div y x
                | otherwise  = -1
