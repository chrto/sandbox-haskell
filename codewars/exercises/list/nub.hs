nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) | x `elem` xs = nub xs
             | otherwise   = x : nub xs


nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x : xs) | x `elem` xs = x : nub' [ e | e <- xs, x /= e ]
              | otherwise   = x : nub' xs
