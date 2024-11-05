-- https://www.codewars.com/kata/5a03b3f6a1c9040084001765/train/haskell

angle :: Int -> Int
angle = (*) 180 . (+) (-2)

--

angle' :: Int -> Int
angle' = (*) 180 . flip (-) 2