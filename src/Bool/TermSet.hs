module Bool.TermSet
    (
      TermSet
    , empty
    , singleton
    , insert
    , member
    , fromList
    , fromPairList
    , toList
    , delete
    , map
    , filter
    , fold
    , covered
    , notCovered
    , intersects
    ) where

import qualified Data.List as List
import           Data.Set  (Set)
import qualified Data.Set  as Set
import           Prelude   hiding (filter, map)

import           Bool      (Value)
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

member :: Term -> TermSet -> Bool
member t = Set.member t . unTermSet

fromList :: [Term] -> TermSet
fromList = TermSet . Set.fromList

fromPairList :: [[(String, Value)]] -> TermSet
fromPairList = fromList . List.map (Term.fromPairList)

toList :: TermSet -> [Term]
toList = Set.toList . unTermSet

-- | Delete a 'Term' from a 'TermSet'.
delete :: Term -> TermSet -> TermSet
delete t = TermSet . Set.delete t . unTermSet

-- | Apply a function to every 'Term' in the 'TermSet'.
map :: (Term -> Term) -> TermSet -> TermSet
map f = TermSet . Set.map f . unTermSet

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
