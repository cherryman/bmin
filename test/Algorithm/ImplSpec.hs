module Algorithm.ImplSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Algorithm.Impl
import           Bool                  (Value (..))
import           Bool.Buf              (FnBuf (..))
import qualified Bool.Buf              as Buf
import           Bool.Function         (Function (..))
import           Bool.TermSet          (TermSet)
import qualified Bool.TermSet          as TermSet

import           Bool.BufSpec          ()
import           Bool.FunctionSpec     ()
import           Bool.TermSetSpec      ()

t :: TermSet
t = TermSet.fromPairList
  [ [("0", T), ("1", T), ("2", T)]
  , [("0", T), ("1", F)]
  ]

tOff :: TermSet
tOff = TermSet.fromPairList
  [ [("0", T), ("1", T)]
  , [("0", F), ("1", F)]
  ]

tExpanded :: TermSet
tExpanded = TermSet.fromPairList
  [ [("2", T)]
  , [("0", T), ("1", F)]
  ]

-- for cdSearch
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

fn :: FnBuf
fn = Buf.fromFn (Function on off)

fnCover :: FnBuf
fnCover = FnBuf (Function c off) c c
  where
    c = TermSet.fromPairList [ [("0", T), ("1", T)] ]

spec :: Spec
spec = do
  describe "expand" $
    it "expands every term without intersect the off-set" $
      let fn' = Function t tOff
          fnBuf'    = FnBuf fn' TermSet.empty t
          fnBufExp' = FnBuf fn' TermSet.empty tExpanded
       in expand fnBuf' `shouldBe` fnBufExp'

  describe "cdSearch" $ do
    it "returns the ON-Set when the OFF-Set is empty" $
      let fn' = Buf.fromFn (Function on TermSet.empty)
          fnCover' = FnBuf (Function on TermSet.empty) on on
       in cdSearch fn' `shouldBe` fnCover'

    it "returns an empty termset when the ON-Set is empty" $
      let fn' = Buf.fromFn (Function TermSet.empty off)
       in cdSearch fn' `shouldBe` fn'

    it "returns the smallest cover for the ON-Set" $
      cdSearch fn `shouldBe` fnCover

    prop "terminates" $
      within 5000 $ -- 5000ms is chosen arbitrarily
        (cdSearch <$> arbitrary) `seq` True
