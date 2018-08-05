module Algorithm.Impl
    (
      Buf(..)
    , emptyBuf
    , insertBuf
    , expand
    ) where

import           Bool.Term    (Term)
import qualified Bool.Term    as Term
import           Bool.TermSet (TermSet, intersects)
import qualified Bool.TermSet as TermSet

data Buf = Buf
    { implicantBuf :: TermSet
    , expandBuf    :: TermSet
    , reduceBuf    :: TermSet
    }

emptyBuf :: Buf
emptyBuf = Buf TermSet.empty TermSet.empty TermSet.empty

insertBuf :: Term -> Buf -> Buf
insertBuf it b@(Buf impl _ _)
    | TermSet.member it impl = b
    | otherwise              = insert it b
  where
    insert :: Term -> Buf -> Buf
    insert t (Buf i e r) =
        Buf (TermSet.insert t i) (TermSet.insert t e) r

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
