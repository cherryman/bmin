{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Bool.FunctionSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck  hiding (Function)

import           Bool.Function
import           Bool.TermSetSpec ()

instance Arbitrary Function where
    arbitrary = Function <$> arbitrary <*> arbitrary

spec :: Spec
spec = return ()
