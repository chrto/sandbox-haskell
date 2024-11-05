module Lib.Search.Imperative
    ( exitIf
    , finito
    , getWords
    , getFileName
    , getFileContent) where

import           System.Exit (die)
import qualified Lib.Search.Common as Common (cleanWord)
import qualified Lib.Common.IO as IO (readWordsFromInput)
import qualified Lib.Common.FS as FS (unsafeGetStorage, unsafeGetFileContent)

type Predicate a = a -> Bool

exitIf :: Predicate a -> String -> a -> IO ()
exitIf predicate errMessage value
  | predicate value = die errMessage
  | otherwise = return ()

finito :: String -> IO ()
finito = die

getWords :: IO [String]
getWords = do
  putStrLn "Specify the words to search:"
  IO.readWordsFromInput []

getFileName :: IO String
getFileName = do
  putStrLn "File to search: "
  fileName <- getLine
  return $ FS.unsafeGetStorage ++ fileName

getFileContent :: String -> IO [String]
getFileContent fileName = (Common.cleanWord <$>) . words <$> FS.unsafeGetFileContent fileName