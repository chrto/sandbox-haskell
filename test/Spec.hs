module Spec where

import           Lib.Common.OrderTree   ( advProp )
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Advanced" $ do
    describe "Tree" $ do
      it "oreder" $ do
        property advProp
