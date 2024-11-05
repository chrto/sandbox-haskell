module Lib.Monad.IOEither where

newtype IOEither l r = IOEither { ioEither :: IO (Either l r) }

instance Functor (IOEither l) where
  -- fmap :: (a -> b) -> IOEither l a -> IOEither l b
  fmap fcn monad = IOEither
    $ do
      either_rv <- ioEither monad
      case either_rv of
        (Right v) -> return (Right (fcn v))
        (Left v)  -> return (Left v)

instance Applicative (IOEither l) where
  -- pure :: a -> IOEither l a
  pure v = IOEither
    $ do
      return (Right v)

  -- (<*>) :: IOEither l (a -> b) -> IOEither l a -> IOEither l b
  monadFcn <*> monadVal = IOEither
    $ do
      either_fcn <- ioEither monadFcn
      either_val <- ioEither monadVal
      case either_fcn of
        (Left v)    -> return (Left v)
        (Right fcn) -> case either_val of
          (Right val) -> return (Right (fcn val))
          (Left val)  -> return (Left val)

instance Monad (IOEither l) where
  -- return :: a -> IOEither l a
  return val = IOEither
    $ do
      return (Right val)

  -- (>>=) :: IOEither l a -> (a -> IOEither l b) -> IOEither l b
  monad >>= f = IOEither
    $ do
      either_val <- ioEither monad
      case either_val of
        Right val -> ioEither (f val)
        Left val  -> return (Left val)
