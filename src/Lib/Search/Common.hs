module Lib.Search.Common (searchInContent, cleanWord) where

import           Data.Char (toLower, isAlphaNum)

searchInContent :: [String] -> [String] -> [(String, String)]
searchInContent [] _ = []
searchInContent (w:ws) content
  | findWord (toLower <$> w) content = (w, "found"):searchInContent ws content
  | otherwise = searchInContent ws content ++ [(w, "NOT found")]
  where
    findWord :: String -> [String] -> Bool
    findWord _ [] = False
    findWord y (x:xs)
      | y == (toLower <$> x) = True
      | otherwise = findWord y xs

cleanWord :: String -> String
cleanWord = reverse . cleanEnd . reverse
  where
    cleanEnd [] = []
    cleanEnd (c:cs)
      | isAlphaNum c = c:cs
      | otherwise = cleanEnd cs