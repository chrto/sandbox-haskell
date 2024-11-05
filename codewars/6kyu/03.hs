-- https://www.codewars.com/kata/54bf1c2cd5b56cc47f0007a1/train/haskell

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

duplicateCount :: String -> Int
duplicateCount xs = map toAsciiDec xs
    |> map toLowerCase
    |> countDup
 where
  countDup :: [Int] -> Int
  countDup [] = 0
  countDup (x : xs) | isDup x xs = 1 + countDup (removeFrom x xs)
                    | otherwise  = countDup xs

  toAsciiDec :: Char -> Int
  toAsciiDec = fromEnum

  toLowerCase :: Int -> Int
  toLowerCase ascii | isUpperCase ascii = ascii + 32
                    | otherwise         = ascii
   where
    isUpperCase :: Int -> Bool
    isUpperCase ascii = 65 <= ascii && 90 >= ascii

  isDup :: Int -> [Int] -> Bool
  isDup _ [] = False
  isDup y (x : xs) | x == y    = True
                   | otherwise = isDup y xs

  removeFrom :: Int -> [Int] -> [Int]
  removeFrom d xs = [ x | x <- xs, x /= d ]
