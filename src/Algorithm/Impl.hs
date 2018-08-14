module Algorithm.Impl
    (
      expand
    ) where

import           Bool.Term    (Term)
import qualified Bool.Term    as Term
import           Bool.TermSet (TermSet, intersects)
import qualified Bool.TermSet as TermSet

-- | Expand a Term until it intersects the given set
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

-- | The implExpand algorithm expands every
-- Term in a TermSet in order to reduce its cost.
expand :: TermSet -- ^ The TermSet to expand
       -> TermSet -- ^ The TermSet which must not be intersected
       -> TermSet
expand ts off = TermSet.map (flip expandTerm $ off) ts
