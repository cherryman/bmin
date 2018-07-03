module Bool.TermSet
    (
      TermSet
    , empty
    , singleton
    , fromList
    , fromPairList
    , insert
    , delete
    , (\\)
    , filter
    , fold
    , covered
    , notCovered
    , intersects
    ) where

import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Prelude   hiding (filter)

import           Bool      (Value, Variable)
import           Bool.Term (Term, covers)
import qualified Bool.Term as Term

newtype TermSet = TermSet
    { unTermSet :: Set Term }
    deriving (Show, Eq, Ord)

empty :: TermSet
empty = TermSet Set.empty

singleton :: Term -> TermSet
singleton = TermSet . Set.singleton

-- | Insert a 'Term' into a 'TermSet'
insert :: Term -> TermSet -> TermSet
insert t = TermSet . Set.insert t . unTermSet

fromList :: [Term] -> TermSet
fromList = TermSet . Set.fromList

fromPairList :: [[(Variable, Value)]] -> TermSet
fromPairList = fromList . map (Term.fromList)

-- | Delete a 'Term' from a 'TermSet'.
delete :: Term -> TermSet -> TermSet
delete t = TermSet . Set.delete t . unTermSet

(\\) :: TermSet -> TermSet -> TermSet
(\\) t0 t1 = TermSet $ (Set.\\) a b
  where
    a = unTermSet t0
    b = unTermSet t1

-- | Filter the 'TermSet' using the given predicate.
filter :: (Term -> Bool) -> TermSet -> TermSet
filter f = TermSet . Set.filter f . unTermSet

-- | Fold over the 'TermSet'.
fold :: (a -> Term -> a) -> a -> TermSet -> a
fold f a = Set.foldl' f a . unTermSet

-- | Return a 'TermSet' containing every 'Term'
-- that is covered by the given 'Term'
covered :: Term -> TermSet -> TermSet
covered t = filter (covers t)

-- | Does the opposite of 'covered'
notCovered :: Term -> TermSet -> TermSet
notCovered t = filter (not . covers t)

-- | Check if a 'TermSet' is covered by a 'Term'.
-- i.e., whether any 'Term' in 'TermSet' is covered
-- by the given 'Term'.
intersects :: Term -> TermSet -> Bool
intersects t = (empty /=) . covered t
