-- https://www.codewars.com/kata/6167e70fc9bd9b00565ffa4e/train/haskell

{-
coffees = [4,3,2]  ->  ?
[4, 9, 13] => 26

coffees = [2,3,4]  ->  ?
[2, 7, 13] => 22

                        => (0,0), [2,3,4]
(0+0,0+2), [3,4]        => (0,2), [3,4]
(0+2, 2+2+3), [4]       => (2,7), [4]
(2+7,7+2+4), []         => (9,13), []
(9+13,0)                => (22,0)
---
coffees = [3,2,5,10,9]  ->  ?
[3, 7, 14, 26, 37] -> 87

coffees = [2,3,5,9,10]  ->  ?
[2, 7, 14, 25, 37] -> 85


---
coffees = [20,5]  ->  ?
[20, 27] -> 47

coffees = [5,20]  ->  ?
[5, 27] -> 32
---

coffees = [20,0,5]  ->  ?
[20, 22, 29] => 71 !!!


-}

import           Data.List                      ( sort )

barista :: [Int] -> Int
barista = sum . scanl1 action' . sort

action' :: Int -> Int -> Int
action' = ((+) 2 .) . (+)


barista''' :: [Int] -> Int
barista''' = sum . calc . sort
 where
  calc :: [Int] -> [Int]
  calc []       = []
  calc (x : xs) = x : calcWithClean (x : xs)
   where
    calcWithClean :: [Int] -> [Int]
    calcWithClean (f : s : xs) = s' : calcWithClean (s' : xs)
      where s' = 2 + f + s
    calcWithClean _ = []

barista'' :: [Int] -> Int
barista'' xs = sum $ calc' $ 0 : sort xs

calc' :: [Int] -> [Int]
calc' (f : s : xs) | f == 0    = s' : calc' (s' : xs)
                  | otherwise = s'' : calc' (s'' : xs)
 where
  s'  = f + s
  s'' = 2 + f + s
calc' _ = []

barista' :: [Int] -> Int
barista' xs = sum' $ foldl op (0, 0) (sort xs)
 where
  op :: (Int, Int) -> Int -> (Int, Int)
  op (0  , 0   ) x = (0, x)
  op (acc, last) x = (acc + last, last + x + 2)

  sum' :: (Int, Int) -> Int
  sum' (x, y) = x + y

