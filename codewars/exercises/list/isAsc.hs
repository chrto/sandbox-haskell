isAsc :: [Int] -> Bool
isAsc (f : s : xs) = f <= s && isAsc (s : xs)
isAsc _            = True
