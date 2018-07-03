module AlgorithmSpec (spec) where

import           Test.Hspec

import           Algorithm
import           Bool         (Literal (..), Value (..))
import qualified Bool.Term    as Term
import qualified Bool.TermSet as TermSet

spec :: Spec
spec = do
  describe "mostFreqLiteral" $
    it "returns the most frequent literal in the termset" $
      let ts = TermSet.fromPairList
            [ [("0", T), ("1", F), ("2", T), ("3", F)]
            , [("0", T), ("1", F), ("2", F)]
            , [("0", T), ("1", T), ("2", F)]
            ]
      in do
        mostFreqLiteral Term.empty ts `shouldBe` (Literal "0" T)
        mostFreqLiteral (Term.fromList [("0", T), ("1", F)]) ts `shouldBe` (Literal "2" F)

  describe "cdSearch" $
    let on = TermSet.fromPairList
          [ [("0", T), ("1", T), ("2", T)]
          , [("0", T), ("1", T), ("2", F)]
          ]
        off = TermSet.fromPairList
          [ [("0", T), ("1", F), ("2", T)]
          , [("0", T), ("1", F), ("2", F)]
          , [("0", F), ("1", T), ("2", T)]
          , [("0", F), ("1", T), ("2", F)]
          , [("0", F), ("1", F), ("2", T)]
          , [("0", F), ("1", F), ("2", F)]
          ]
        cover = TermSet.fromPairList
          [ [("0", T), ("1", T)] ]
    in do
      it "returns the ON-Set when the OFF-Set is empty" $
        cdSearch on TermSet.empty `shouldBe` on

      it "returns an empty termset when the ON-Set is empty" $
        cdSearch TermSet.empty off `shouldBe` TermSet.empty

      it "returns the smallest cover for the ON-Set" $
        cdSearch on off `shouldBe` cover
