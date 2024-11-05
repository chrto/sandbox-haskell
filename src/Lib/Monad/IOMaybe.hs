module Lib.Monad.IOMaybe (IOMaybe(IOMaybe), ioMaybe, doer) where

newtype IOMaybe a = IOMaybe { ioMaybe :: IO (Maybe a) }

-- printIOM :: Show a => IOMaybe a -> IOMaybe a
-- printIOM :: Show a => IOMaybe a -> IOMaybe a
-- printIOM io_maybe_v = io_maybe_v >>= mapM_ print
instance Functor IOMaybe where
  -- fmap :: (a -> b) -> IOMaybe a -> IOMaybe b
  fmap fcn (IOMaybe io_maybe_val) = IOMaybe
    $ do
      maybe_val <- io_maybe_val
      case maybe_val of
        (Just val) -> return $ Just $ fcn val
        Nothing    -> return Nothing

-- x <$ monad
instance Applicative IOMaybe where
  -- pure :: a -> IOMaybe a
  pure val = IOMaybe
    $ do
      return (Just val)

  -- (<*>) :: IOMaybe (a -> b) -> IOMaybe a -> IOMaybe b
  monadFcn <*> monadVal = IOMaybe
    $ do
      maybe_fcn <- ioMaybe monadFcn
      maybe_val <- ioMaybe monadVal
      case maybe_fcn of
        Nothing    -> return Nothing
        (Just fcn) -> case maybe_val of
          (Just val) -> return (Just (fcn val))
          Nothing    -> return Nothing

instance Monad IOMaybe where
  -- return :: a -> IOMaybe a
  return val = IOMaybe
    $ do
      return (Just val)

  -- (>>=) :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
  monad >>= f = IOMaybe
    $ do
      maybe_v <- ioMaybe monad
      case maybe_v of
        Just v  -> ioMaybe (f v)
        Nothing -> return Nothing

  -- (>>=) :: IOMaybe a -> (a -> IOMaybe b) -> IOMaybe b
  -- (<&>) :: IOMaybe a -> (a -> b) -> IOMaybe b
  -- ?? :: IOMaybe a ->(a -> IO a) -> IOMaybe b
  -- ?? :: IOMaybe a ->(a -> IO ()) -> IOMaybe a
  -- ?? :: IOMaybe a ->(a -> Maybe a) -> IOMaybe b
  -- aaaa :: IOMaybe a -> (a -> IO ()) -> IOMaybe a
-- doer :: IOMaybe a -> (a -> IO ()) -> IOMaybe a
-- chainIO :: IOMaybe a -> (a -> IO ()) -> IOMaybe a
doer :: IOMaybe a -> (a -> IO b) -> IOMaybe a
monad `doer` fn = IOMaybe
  $ do
    maybe_val <- ioMaybe monad
    case maybe_val of
      Just val -> do
        _ <- fn val
        return $ Just val
      Nothing  -> return Nothing
