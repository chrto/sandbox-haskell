module SearchFile where

import qualified Exercises.Advanced.SearchFileLib as SearchFileLib (getWords
                                                                  , getFileName, getFileContent, searchInContent, printResult)
import qualified Exercises.Advanced.Either as EitherHelper (ioBind, caseOf)
import qualified Exercises.Advanced.Maybe as MaybeHelper (ioLift, makeSure)
import           Data.Functor ((<&>))
import qualified System.Environment as ENV (lookupEnv, getArgs)
import qualified System.Exit as Exit (exitSuccess)

printVersion :: IO ()
printVersion = ENV.getArgs
  <&> (MaybeHelper.makeSure
         (\args -> "-v" `elem` args || "--version" `elem` args))
  >>= (MaybeHelper.ioLift (\_ -> putStr "search file v1.2.0"))
  >>= (MaybeHelper.ioLift (\_ -> Exit.exitSuccess))
  >>= (const $ return $ ())

main :: IO ()
main = printVersion >> ENV.lookupEnv "USER"
  <&> (<&> (\name -> "Hello " ++ name))
  >>= (MaybeHelper.ioLift putStrLn)
  >> SearchFileLib.getWords
  >>= (EitherHelper.ioBind
         (\w -> SearchFileLib.getFileName
          >>= (EitherHelper.ioBind SearchFileLib.getFileContent)
          <&> (<&> SearchFileLib.searchInContent w)))
  >>= (EitherHelper.caseOf SearchFileLib.printResult print)