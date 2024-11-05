module SearchFileImperative where

import qualified Lib.Search.Common as Common (searchInContent)
import qualified Lib.Common.IO as IO (printResultTerminal, printResultFile)
import qualified Lib.Search.Imperative as SearchFile (getWords, exitIf
                                                    , getFileName
                                                    , getFileContent)

main :: IO ()
main = do
  ws <- SearchFile.getWords
  SearchFile.exitIf null "Words to search have not been specified!" ws
  fileName <- SearchFile.getFileName
  SearchFile.exitIf null "File name has not been specified!" fileName
  fileContent <- SearchFile.getFileContent fileName
  SearchFile.exitIf null "File is Empty!" fileContent
  let result = Common.searchInContent ws fileContent
  IO.printResultTerminal result
  IO.printResultFile result