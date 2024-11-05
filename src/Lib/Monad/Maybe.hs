module Lib.Monad.Maybe (makeSure, ioLift, ioBind, caseOf) where

import           GHC.Base (returnIO)
import           Data.Functor ((<&>))

type Predicate a = a -> Bool

makeSure :: Predicate a -> a -> Maybe a
makeSure predicate value
  | predicate value = Just value
  | otherwise = Nothing

caseOf :: (a -> b) -> b -> Maybe a -> b
caseOf f def maybeVal = case maybeVal of
  (Just v) -> f v
  Nothing  -> def

ioLift :: (a -> IO b) -> Maybe a -> IO (Maybe b)
ioLift f_io maybeVal = extractIO $ f_io <$> maybeVal

extractIO :: Maybe (IO a) -> IO (Maybe a)
extractIO maybeIO = case maybeIO of
  Just io_v -> io_v <&> Just
  Nothing   -> returnIO Nothing

ioBind :: (a -> IO (Maybe b)) -> Maybe a -> IO (Maybe b)
ioBind f_io_mb maybeVal = extractIOEither $ f_io_mb <$> maybeVal

extractIOEither :: Maybe (IO (Maybe a)) -> IO (Maybe a)
extractIOEither maybeIO = case maybeIO of
  (Just io_v) -> io_v
  Nothing     -> returnIO Nothing
