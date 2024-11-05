-- https://www.codewars.com/kata/5f70c883e10f9e0001c89673/train/haskell

gravityFlip :: Char -> [Int] -> [Int]
gravityFlip 'R' = sortBox
 where
  sortBox :: (Ord a) => [a] -> [a]
  sortBox []       = []
  sortBox (p : xs) = sortBox lesser ++ p : sortBox greater
   where
    lesser  = [ x | x <- xs, x <= p ]
    greater = [ x | x <- xs, x > p ]
gravityFlip 'L' = reverseBox . gravityFlip 'R'
 where
  reverseBox :: [Int] -> [Int]
  reverseBox []       = []
  reverseBox (x : xs) = reverseBox xs ++ [x]
gravityFlip _ = error "wrong gravity"
