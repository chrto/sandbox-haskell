-- https://www.codewars.com/kata/515de9ae9dcfc28eb6000001/train/haskell

solution :: String -> [String]
solution xs = map (\(_, x, y) -> [x, y]) . filter (\(index, _, _) -> even index) $ zip3 [0 ..] xs (tail xs ++ ['_'])

-- solution xs = map (\(_, x, y) -> [x, y]) $ filter (\(index, _, _) -> even index) $ zip3 [0 ..] xs (tail xs)
-- solution xs = map (\(_, x, y) -> [x, y]) $ filter (\(index, _, _) -> even index) $ (\xs -> zip3 [0 ..] xs (tail xs)) (if even $ length xs then xs else xs ++ ['_'])
-- solution xs = map (\(_, (x, y)) -> [x, y]) $ filter (\(x, (_, _)) -> even x) $ zip [0 ..] $ (\xs -> zip xs (tail xs)) (if even $ length xs then xs else xs ++ ['_'])