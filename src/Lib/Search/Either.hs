module Lib.Search.Either (getWords, getFileName, getFileContent) where

import           Data.Functor ((<&>))
import qualified Lib.Monad.Either as Either (makeSure)
import qualified Lib.Search.Common as Common (cleanWord)
import qualified Lib.Common.IO as CommonIO (readWordsFromInput)
import qualified Lib.Common.FS as FS (getFileContent, unsafeGetStorage)

getWords :: IO (Either String [String])
getWords = putStrLn "Specify the words to search:"
  >> CommonIO.readWordsFromInput []
  <&> Either.makeSure (not . null) "Words to search have not been specified!"

getFileName :: IO (Either String String)
getFileName = putStrLn "File to search: " >> getLine
  <&> Either.makeSure (not . null) "File name has not been specified!"
  <&> (<&> (++) FS.unsafeGetStorage)

getFileContent :: String -> IO (Either String [String])
getFileContent fileName =
  FS.getFileContent fileName <&> (<&> words) <&> (<&> (<&> Common.cleanWord))
--
-- searchInFile :: [String] -> IO (Either String [(String, String)])
-- searchInFile ws = putStrLn "File to search: " >> getLine
--   <&> makeSure (not . null) "File name has not been specified!"
--   >>= (ioLift readFile)
--   <&> (>>= makeSure (not . null) "File is Empty!")
--   <&> (<&> words)
--   <&> (<&> searchInContent ws)
