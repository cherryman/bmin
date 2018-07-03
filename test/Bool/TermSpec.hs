{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Bool.TermSpec (spec) where

import           Prelude               hiding (lookup)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Bool
import           Bool.Term

import           BoolSpec              ()


instance Arbitrary Term where
    arbitrary = frequency
        [ (1, return empty)
        , (20, insert <$> arbitrary <*> arbitrary)
        ]

spec :: Spec
spec = do
  describe "lookup" $ do
    it "returns X when element is not present" $
      lookup "" empty `shouldBe` X

    it "returns the element related to the key" $ do
      lookup "0" (singleton $ Literal "0" F) `shouldBe` F
      lookup "1" (singleton $ Literal "1" T) `shouldBe` T

  describe "insert" $ do
    it "does nothing when inserting X when key is not present" $
      insert (Literal "" X) empty `shouldBe` empty

    it "deletes the literal when replacing with X" $
      insert (Literal "" X) (singleton $ Literal "" T) `shouldBe` empty

    it "inserts an element" $ do
      insert (Literal "0" F) empty `shouldBe` singleton (Literal "0" F)
      insert (Literal "1" T) empty `shouldBe` singleton (Literal "1" T)

  describe "delete" $ do
    prop "deletes a literal from the term" $
      \l -> delete l (singleton l) == empty

    prop "does nothing if the literal is not present" $
      \l -> do
        t <- arbitrary :: Gen Term
        return $
          not (member l t) ==> delete l t == t

  describe "member" $
    prop "checks if a literal is a member of a term" $
      \l -> member l . insert l <$> arbitrary

  describe "fold" $
    prop "folds over every literal in the term" $ do
      \t -> fold (flip delete) t t == empty

  describe "covers" $ do
    prop "true when the first argument is empty" $
      \t -> covers empty t

    prop "false when the second argument is empty" $
      \t -> t /= empty ==> not (covers t empty)

    it "checks if the second term is covered by the first" $ do
      t0 `covers` t1 `shouldBe` True
      t1 `covers` t0 `shouldBe` False
        where
        t0 = fromList [("0",T)]
        t1 = fromList [("0",T), ("1",F)]
