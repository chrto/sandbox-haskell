module SearchFileIOM where

-- import qualified Lib.Search.Either as SearchFileLib (getWords, getFileName
--                                                    , getFileContent)
-- import qualified Lib.Search.Common as Common (searchInContent)
import qualified Lib.Common.MaybeIO as MaybeIO (getUsername)
-- import qualified Lib.Monad.IOEither as IOEither (IOEither(IOEither))
import           Data.Functor ((<&>))
import qualified Lib.Monad.IOMaybe as IOMaybe (IOMaybe(IOMaybe), ioMaybe)

-- import qualified Lib.Monad.Either as Either (ioBind, caseOf)
-- import qualified Lib.Monad.Maybe as Maybe (ioLift, makeSure)
-- import           Data.Functor ((<&>))
-- import qualified System.Environment as ENV (lookupEnv, getArgs)
-- import qualified System.Exit as Exit (exitSuccess)
-- printVersion :: IO ()
-- printVersion = ENV.getArgs
--   <&> (Maybe.makeSure (\args -> "-v" `elem` args || "--version" `elem` args))
--   >>= (Maybe.ioLift (\_ -> putStr "search file v1.2.0"))
--   >>= (Maybe.ioLift (\_ -> Exit.exitSuccess))
--   >>= (const $ return $ ())
greeting :: IOMaybe.IOMaybe String
greeting = MaybeIO.getUsername <&> ("Hello " ++)

add' :: Int -> Int
add' x = x + 1

-- main :: IO ()
main :: IO (Maybe String)
main = IOMaybe.ioMaybe
  $ MaybeIO.getUsername
  <&> ("Hello " ++)
  <&> (\x -> x <$ IOMaybe.IOMaybe
       $ do
         y <- print x
         return $ Just y)
-- >>=<< print
-- IOMaybe.caseOf
-- MaybeIO.getUsername
-- <&> ("Hello " ++)
-- x <-  (("Hello " ++) <$> MaybeIO.getUsername)
-- return ()
-- main = printVersion >> ENV.lookupEnv "USER"
-- <&> (<&> (\name -> "Hello " ++ name))
-- >>= (Maybe.ioLift putStrLn)
-- >> SearchFileLib.getWords
-- >>= (Either.ioBind
--        (\w -> SearchFileLib.getFileName
--         >>= (Either.ioBind SearchFileLib.getFileContent)
--         <&> (<&> Common.searchInContent w)))
-- >>= (Either.caseOf IO.printResultTerminal print)