module Lib.Common.FS
    ( getStorage
    , getFileContent
    , unsafeGetStorage
    , unsafeGetFileContent
    , writetoOutFile
    , appendtoOutFile) where

import           System.Directory (doesFileExist)
import           Lib.Monad.Either as Either (makeSure, ioLift)
import           System.IO (hPutStr, hPutStrLn, openFile, hClose
                          , IOMode(AppendMode))
import           Lib.Common.OS (isWin)
import           Data.Functor ((<&>))

getStorage :: Either String FilePath
getStorage = case unsafeGetStorage of
  ""   -> Left "Windows is not supported!"
  path -> Right path

unsafeGetStorage :: FilePath
unsafeGetStorage
  | not isWin = "storage/"
  | otherwise = ""

unsafeGetOutFile :: FilePath
unsafeGetOutFile = unsafeGetStorage ++ "output"

getFileContent :: FilePath -> IO (Either String String)
getFileContent fileName = doesFileExist fileName
  <&> Either.makeSure id "File does not exists!"
  <&> (<&> const fileName)
  >>= (Either.ioLift unsafeGetFileContent)
  <&> (>>= Either.makeSure (not . null) "File is empty")

unsafeGetFileContent :: FilePath -> IO String
unsafeGetFileContent = readFile

writetoOutFile :: (Show a) => a -> IO ()
writetoOutFile content = writeFile unsafeGetOutFile (show content)

appendtoOutFile :: Bool -> String -> IO ()
appendtoOutFile newLine content = do
  fileHandle <- openFile unsafeGetOutFile AppendMode
  case newLine of
    True -> hPutStrLn fileHandle (show content)
    _    -> hPutStr fileHandle (show content)
  hClose fileHandle
