{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Bool.BufSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Bool.Buf
import           Bool.FunctionSpec ()
import           Bool.TermSetSpec  ()

instance Arbitrary FnBuf where
    arbitrary = FnBuf <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = return ()
