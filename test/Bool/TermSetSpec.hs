{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Bool.TermSetSpec (spec) where

import           Prelude               hiding (filter)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Bool                  (Literal (..), Value (..))
import qualified Bool.Term             as Term
import           Bool.TermSet

import           Bool.TermSpec         ()


instance Arbitrary TermSet where
    arbitrary = frequency
        [ (1, return empty)
        , (10, insert <$> arbitrary <*> arbitrary)
        ]

spec :: Spec
spec = do
  describe "insert" $
    it "inserts a term into a termset" $
      let t = Term.singleton (Literal "" T)
      in  insert t empty `shouldBe` singleton t

  describe "delete" $
    it "deletes a term from a termset" $
      let t = Term.singleton (Literal "" T)
      in  delete t (singleton t) `shouldBe` empty

  describe "filter" $
    it "filters a termset given a predicate" $
      let t  = Term.empty
      in do
        filter (==t) (singleton t) `shouldBe` singleton t
        filter (const True) (singleton t) `shouldBe` singleton t
        filter (const False) (singleton t) `shouldBe` empty

  describe "fold" $
    prop "folds over every term in the termset" $
      \ts -> fold (flip delete) ts ts == empty

  describe "covered" $
    prop "returns the entire termset for an empty term" $
      \ts -> covered Term.empty ts == ts

  describe "notCovered" $
    prop "returns empty for an empty term" $
      \ts -> notCovered Term.empty ts == empty
