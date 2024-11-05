import           Control.Monad                  ( when )
import           Data.Char                      ( toUpper )

hello :: [Char] -> [Char]
hello name = "Hello " ++ name ++ "!"

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- hello <$> (map toUpper) <$> getLine
  putStrLn name
  -- putStrLn $ hello name
  putStrLn "Quit? [y/N]"
  -- q <- fmap (/= "y") getLine
  q <- (/= "y") <$> getLine
  when q greet

main :: IO ()
main = do
  greet
