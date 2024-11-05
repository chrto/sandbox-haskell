-- https://www.codewars.com/kata/56484848ba95170a8000004d/train/haskell

import Data.Functor.Compose

gps :: Int -> [Double] -> Int
gps t xs = maybe 0 maximum (getCompose (floor . (/ fromIntegral t) . (* 3600) <$> Compose (if length xs > 1 then Just (zipWith (-) (tail xs) xs) else Nothing)))
