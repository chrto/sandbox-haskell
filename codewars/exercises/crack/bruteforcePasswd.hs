import Data.Maybe
import Data.Char (chr)

bruteforce :: (a -> Bool) -> [a] -> Maybe a
bruteforce f xs
  | null result = Nothing
  | otherwise = Just $ head result
  where
    result = mapMaybe bruteforce' xs
    -- test one instance
    bruteforce' x
      | f x = Just x
      | otherwise = Nothing

generatorString :: Int -> [String]
generatorString 0 = [""]
generatorString deep = concatMap (\x -> map (\ys -> (x:ys)) $ generatorString (deep - 1)) alphanumeric
-- generatorString deep = concatMap (\x -> map (\ys -> (x:ys)) nextgen) ['a'..'z']
--   where nextgen = generatorString (deep - 1)

lowerCaseChars :: [Char]
lowerCaseChars = ['a'..'z']

alphabetical :: [Char]
alphabetical = ['a' .. 'z'] ++ ['A' .. 'Z']

numeric :: [Char]
numeric = map chr [48 .. 57]

alphanumeric :: [Char]
alphanumeric = alphabetical ++ numeric

allChars :: [Char]
allChars = map chr [32 .. 126]

main :: IO ()
main = do
  putStrLn $ fromJust $ bruteforce ((==) "xrTo12fG") (generatorString 8)

  {-
  build: ghc -O2 -fno-full-laziness -rtsopts bruteforce
  run: bruteforce
  -}