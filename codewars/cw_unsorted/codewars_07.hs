-- https://www.codewars.com/kata/554b4ac871d6813a03000035/train/haskell

highAndLow :: String -> String
highAndLow = unwords . map show . (\xs -> maximum xs : [minimum xs]) . map (\x -> read x :: Int) . words

--

highAndLow' :: String -> String
highAndLow' = unwords . map show . sequence [maximum,minimum] . map (read ::String->Int) . words

--

(|>) x f = f x

highAndLow'' :: String -> String
highAndLow'' input =
    input
    |> words
    |> map (\s -> read s :: Int)
    |> foldr (\e (min', max') -> (min min' e, max max' e)) (maxBound, minBound)
    |> \(min', max') -> show max' ++ " " ++ show min'