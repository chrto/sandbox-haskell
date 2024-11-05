module Lib.Common.IO
    ( printResult
    , printResultTerminal
    , printResultFile
    , sniffArray
    , sniff
    , readWordsFromInput) where

import           Lib.Common.FS (appendtoOutFile)

readWordsFromInput :: [String] -> IO [String]
readWordsFromInput ws = do
  w <- getLine
  addWord w
  where
    addWord w
      | null w = return ws
      | otherwise = readWordsFromInput (ws ++ [w])

putStrToTerminal :: Bool -> String -> IO ()
putStrToTerminal True = putStrLn
putStrToTerminal _ = putStr

printResultTerminal :: [(String, String)] -> IO ()
printResultTerminal = printResult putStrToTerminal

printResultFile :: [(String, String)] -> IO ()
printResultFile = printResult appendtoOutFile

printResult :: (Bool -> String -> IO ()) -> [(String, String)] -> IO ()
printResult _ [] = return ()
printResult output ((w, r):xs) = do
  output False ("\"" ++ w ++ "\" ")
  output True r
  printResult output xs

sniffArray :: (Show a) => [a] -> IO [a]
sniffArray = mapM sniff

sniff :: (Show a) => a -> IO a
sniff v = do
  print v
  return v