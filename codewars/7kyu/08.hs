-- https://www.codewars.com/kata/61123a6f2446320021db987d/train/haskell

prevMultOfThree :: Int -> Maybe Int
prevMultOfThree n | n < 3          = Nothing
                  | n `mod` 3 == 0 = Just n
                  | otherwise      = prevMultOfThree (n `div` 10)
