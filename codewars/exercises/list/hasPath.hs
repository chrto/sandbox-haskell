nodes :: [(Int, Int)]
nodes = [(1, 2), (2, 3), (3, 2), (4, 3), (4, 5)]

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath ns from to
  | from > to = hasNode (from, from - 1) ns && hasPath ns (from - 1) to
  | from < to = hasNode (from, from + 1) ns && hasPath ns (from + 1) to
  | otherwise  = True
 where
  hasNode :: (Int, Int) -> [(Int, Int)] -> Bool
  hasNode _ [] = False
  hasNode (from, to) ((f, t) : ns) =
    from == f && to == t || hasNode (from, to) ns
