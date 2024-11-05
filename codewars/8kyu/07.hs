-- https://www.codewars.com/kata/555086d53eac039a2a000083/train/haskell

inlove :: Int -> Int -> Bool
inlove = (odd .) . (+)

inlove'' :: Int -> Int -> Bool
inlove'' a b | even' a   = odd' b
             | otherwise = even' b
 where
  even' :: Int -> Bool
  even' = (== 0) . flip mod 2

  odd' :: Int -> Bool
  odd' = not . even'

inlove' :: Int -> Int -> Bool
inlove' a b = even a && odd b || odd a && even b
