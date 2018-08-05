module AlgorithmSpec (spec) where

import qualified Data.Map              as Map

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (cover)

import           Algorithm
import           Bool
    (Literal (..), Value (..), Variable (..), newLit)
import           Bool.TermSet          (TermSet)
import qualified Bool.TermSet          as TermSet

import           Bool.TermSetSpec      ()

-- Variables used to test
on :: TermSet
on = TermSet.fromPairList
  [ [("0", T), ("1", T), ("2", T)]
  , [("0", T), ("1", T), ("2", F)]
  ]

off :: TermSet
off = TermSet.fromPairList
  [ [("0", T), ("1", F), ("2", T)]
  , [("0", T), ("1", F), ("2", F)]
  , [("0", F), ("1", T), ("2", T)]
  , [("0", F), ("1", T), ("2", F)]
  , [("0", F), ("1", F), ("2", T)]
  , [("0", F), ("1", F), ("2", F)]
  ]

cover :: TermSet
cover = TermSet.fromPairList
  [ [("0", T), ("1", T)] ]

spec :: Spec
spec = do
  describe "countLiterals" $
    it "returns a count of every literal in the term" $
      let lc = Map.fromList
            [ (newLit "0" T, 2)
            , (newLit "0" F, 4)
            , (newLit "1" T, 2)
            , (newLit "1" F, 4)
            , (newLit "2" T, 3)
            , (newLit "2" F, 3)
            ]
       in countLiterals off `shouldBe` lc

  describe "mostFreqLiterals" $
    it "returns the most frequent literals in the termset" $ do
      mostFreqLiterals [] on `shouldBe`
          [(newLit "0" T), (newLit "1" T)]

      mostFreqLiterals [] off `shouldBe`
          [(newLit "0" F), (newLit "1" F)]

      mostFreqLiterals [Variable "0", Variable "1"] off `shouldBe`
          [(newLit "2" F), (newLit "2" T)]

  describe "cdSearch" $ do
    it "returns the ON-Set when the OFF-Set is empty" $
      cdSearch on TermSet.empty `shouldBe` on

    it "returns an empty termset when the ON-Set is empty" $
      cdSearch TermSet.empty off `shouldBe` TermSet.empty

    it "returns the smallest cover for the ON-Set" $
      cdSearch on off `shouldBe` cover

    prop "terminates" $
      within 5000 $ -- 5000ms is chosen arbitrarily
        (cdSearch <$> arbitrary <*> arbitrary) `seq` True
