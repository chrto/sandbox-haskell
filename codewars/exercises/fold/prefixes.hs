prefixes :: [a] -> [[a]]
prefixes = foldr f [] where f = \x acc -> [x] : ((x :) <$> acc)

prefixes' :: [a] -> [[a]]
prefixes' = foldr f [] where f = \x acc -> [x] : map (x :) acc





      --   f = \x acc -> [x] : ((x :) <$> acc)


            -- f = \x acc ->  (x :) <$> acc
    --  f = \x acc -> (++ [x]) <$> acc

-- accdbg :: [a]
-- accdbg = []

-- dbg::a -> [a]->[a]
-- dbg = \x acc -> (:) x acc
