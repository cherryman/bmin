{-# OPTIONS_GHC -fno-warn-orphans  #-}
module BoolSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Bool            (Literal (..), Value (..), Variable (..))

instance Arbitrary Variable where
  arbitrary = Variable <$> arbitrary

instance Arbitrary Value where
    arbitrary = elements [F,X,T]
    shrink T = [F,X]
    shrink F = [X]
    shrink X = []

instance Arbitrary Literal where
    arbitrary = Literal <$> arbitrary <*> arbitrary

spec :: Spec
spec = return ()
