import GHC.Exts.Heap (GenClosure (DoubleClosure))

-- https://www.codewars.com/kata/563b662a59afc2b5120000c6/train/haskell

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p
  | p0 >= p = 0
  | otherwise = 1 + nbYear p0' percent aug p
  where
    p0' =
      (+) aug $
        floor $
          (*) (fromIntegral p0) $
            (+) 1 $
              (/) percent 100

nbYear' :: Int -> Double -> Int -> Int -> Int
nbYear' p0 percent aug p =
  length $
    takeWhile (< p) $
      iterate
        ( (+ aug)
            . floor
            . (* (1 + percent / 100))
            . fromIntegral
        )
        p0
