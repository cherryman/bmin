module Algorithm.ImplSpec (spec) where

import           Test.Hspec

import           Algorithm.Impl
import           Bool           (Value (..))
import           Bool.TermSet   (TermSet)
import qualified Bool.TermSet   as TermSet

t :: TermSet
t = TermSet.fromPairList
  [ [("0", T), ("1", T), ("2", T)]
  , [("0", T), ("1", F)]
  ]

t_expanded :: TermSet
t_expanded = TermSet.fromPairList
  [ [("2", T)]
  , [("0", T), ("1", F)]
  ]

off :: TermSet
off = TermSet.fromPairList
  [ [("0", T), ("1", T)]
  , [("0", F), ("1", F)]
  ]

spec :: Spec
spec = do
    describe "expand" $
      it "expands every term in a termset without intersect the off-set" $
        expand t off `shouldBe` t_expanded
