module Lib.Common.MaybeIO
    (getUsername) where

import qualified System.Environment as ENV (lookupEnv)
import qualified Lib.Monad.IOMaybe as IOMaybe (IOMaybe(IOMaybe))

getUsername:: IOMaybe.IOMaybe String
getUsername = IOMaybe.IOMaybe  (ENV.lookupEnv "USER")