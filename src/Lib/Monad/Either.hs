module Lib.Monad.Either (makeSure, ioLift, ioBind, caseOf, ignoreResult) where

import           GHC.Base (returnIO)
import           Data.Functor ((<&>))
import qualified Lib.Common.Pred as Pred (Predicate)

makeSure :: Pred.Predicate a -> l -> a -> Either l a
makeSure predicate left value
  | predicate value = Right value
  | otherwise = Left left

caseOf :: (r -> a) -> (l -> a) -> Either l r -> a
caseOf fr fl valOrErr = case valOrErr of
  (Right r) -> fr r
  (Left l)  -> fl l

ioLift :: (a -> IO b) -> Either l a -> IO (Either l b)
ioLift f_io vOrE = extractIO $ f_io <$> vOrE

extractIO :: Either l (IO a) -> IO (Either l a)
extractIO ioOrErr = case ioOrErr of
  Right io_v -> io_v <&> Right
  Left err   -> returnIO (Left err)

ioBind :: (a -> IO (Either l b)) -> Either l a -> IO (Either l b)
ioBind f_io_e vOrE = extractIOEither $ f_io_e <$> vOrE

extractIOEither :: Either l (IO (Either l a)) -> IO (Either l a)
extractIOEither ioOrErr = case ioOrErr of
  (Right io_vOrErr) -> io_vOrErr
  (Left err)        -> returnIO (Left err)

ignoreResult :: (a -> IO b) -> Either l a -> IO (Either l a)
ignoreResult fn vOrE = case vOrE of
  Right v -> fn v >> returnIO (Right v)
  Left e  -> return $ Left e
