module SearchFileFuncional where

import qualified Lib.Search.Either as SearchFileLib (getWords
                                                                  , getFileName, getFileContent)
import qualified Lib.Search.Common as Common (searchInContent)
import qualified Lib.Common.IO as IO (printResultTerminal)
import qualified Lib.Monad.Either as Either (ioBind, caseOf, ioLift)
import qualified Lib.Monad.Maybe as Maybe (ioLift, makeSure)
import           Data.Functor ((<&>))
import qualified System.Environment as ENV (lookupEnv, getArgs)
import qualified System.Exit as Exit (exitSuccess)

printVersion :: IO (Maybe a)
printVersion =
  ENV.getArgs
  <&> Maybe.makeSure
         (\args -> "-v" `elem` args || "--version" `elem` args)
  >>= Maybe.ioLift  (const $ putStr "search file v1.2.0")
  >>= Maybe.ioLift (const Exit.exitSuccess)
-- printVersion = ENV.getArgs
--   <&> (Maybe.makeSure
--          (\args -> "-v" `elem` args || "--version" `elem` args))
--   >>= (Maybe.ioLift (const $  putStr "search file v1.2.0"))
--   >>= (Maybe.ioLift (const $ Exit.exitSuccess))
--   >>= (const $ return ())

main :: IO (Either String ())
main =
  printVersion
  >> ENV.lookupEnv "USER"
  <&> (<&> (\name -> "Hello " ++ name))
  >>= (Maybe.ioLift putStrLn)
  >> SearchFileLib.getWords
  >>= (Either.ioBind
         (\w -> SearchFileLib.getFileName
          >>= (Either.ioBind SearchFileLib.getFileContent)
          <&> (<&> Common.searchInContent w)))
  >>= (Either.ioLift print)
  -- >>= (Either.caseOf IO.printResultTerminal print)