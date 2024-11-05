findNextSquare :: Integer -> Integer
findNextSquare = findNext 0
 where
  findNext :: Integer -> Integer -> Integer
  findNext s n | s ^ 2 > n  = -1
               | s ^ 2 == n = (s + 1) ^ 2
               | otherwise  = findNext (s + 1) n

findNextSquare'' n | n == floorSquare ^ 2 = (floorSquare + 1) ^ 2
                   | otherwise            = -1
  where floorSquare = floor (sqrt (fromIntegral n))

findNextSquare' :: Integer -> Integer
findNextSquare' x | x == y    = z
                  | otherwise = -1
  where (y : z : _) = dropWhile (< x) $ (^ 2) <$> [0 ..]
