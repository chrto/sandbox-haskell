-- https://www.codewars.com/kata/578553c3a1b8d5c40300037c/train/haskell

toNumber :: [Int] -> Int
toNumber xs =
  sum $ zipWith (*) [ 2 ^ x | x <- [length xs - 1, length xs - 2 ..] ] xs

toNumber'' :: [Int] -> Int
toNumber'' xs =
  sum
    $ [ case x of
          (1, e) -> 2 ^ e
          (_, _) -> 0
      | x <- zip xs [length xs - 1, length xs - 2 ..]
      , fst x == 1
      ]

toNumber' :: [Int] -> Int
toNumber' []       = 0
toNumber' (x : xs) = 2 ^ length xs * x + toNumber' xs
