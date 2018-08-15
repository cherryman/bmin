module Algorithm.Impl
    (
      cdSearch
    , expand
    ) where

import           Algorithm     (mostFreqLiterals)
import           Bool          (Literal)
import           Bool.Buf      (FnBuf (..))
import           Bool.Function (Function (..))
import           Bool.Term     (Term)
import qualified Bool.Term     as Term
import           Bool.TermSet  (TermSet, intersects)
import qualified Bool.TermSet  as TermSet


cdSearch :: FnBuf -> FnBuf
cdSearch (FnBuf (Function on off) ib eb) =
    FnBuf (Function newOn off) ib' eb'
  where
    newOn = cdSearchA on off       -- Generate new cover
    ib'   = TermSet.union ib newOn -- Store every generated implicant
    eb'   = TermSet.fold           -- Only store new implicants
        (\acc t ->
            if TermSet.member t ib
                then acc
                else TermSet.insert t acc)
        eb newOn

-- Build a 'TermSet' which covers the entire ON-Set without
-- intersect the OFF-Set.
cdSearchA :: TermSet -- ^ ON-Set
          -> TermSet -- ^ OFF-Set
          -> TermSet
cdSearchA on off
    | on  == TermSet.empty = TermSet.empty
    | off == TermSet.empty = on
    | otherwise =
        TermSet.insert newTerm (cdSearchA newOn off)
  where
    pickNextLiteral :: Term -> [Literal] -> Literal
    pickNextLiteral _ [] = error "Cannot pick literal from empty list"
    pickNextLiteral _ [x] = x
    pickNextLiteral t (l:ls)
        | oldCover /= newCover = l
        | otherwise            = pickNextLiteral t ls
      where
        oldCover = TermSet.covered t off
        newCover = TermSet.covered (Term.insert l t) off

    -- Search for the most frequent literal in the
    -- ON-Set that doesn't intesect the OFF-Set.
    nextTerm :: Term -> TermSet -> Term
    nextTerm t ts
        | nt `intersects` off = nextTerm nt cts
        | otherwise           = nt
      where
        vars = Term.varList t
        nextLiteral = pickNextLiteral t (mostFreqLiterals vars ts)
        nt   = Term.insert nextLiteral t
        cts  = TermSet.covered nt ts

    newTerm = nextTerm Term.empty on
    newOn   = TermSet.notCovered newTerm on

expand :: FnBuf -> FnBuf
expand (FnBuf (Function on off) ib eb) =
    FnBuf (Function on off) ib eb'
  where
    eb' = expandA eb off

-- | The implExpand algorithm expands every
-- Term in a TermSet in order to reduce its cost.
expandA :: TermSet -- ^ The TermSet to expand
        -> TermSet -- ^ The TermSet which must not be intersected
        -> TermSet
expandA ts off = TermSet.map (flip expandTerm $ off) ts

-- | Expand a Term until it intersects the given set
-- Used mainly by expand.
expandTerm :: Term    -- ^ Term to expand
           -> TermSet -- ^ TermSet which must not be intersected
           -> Term
expandTerm t off = Term.fold
    (\a l ->
        let new = Term.delete l a
         in if new `intersects` off
                then a
                else new)
    t t
