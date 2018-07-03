module Algorithm
    (
      cdSearch
    )
    where

import           Data.List    (maximumBy)
import           Data.Map     (Map)
import qualified Data.Map     as Map
import           Data.Maybe   (fromMaybe)

import           Bool         (Literal (..))
import           Bool.Term    (Term)
import qualified Bool.Term    as Term
import           Bool.TermSet (TermSet, intersects)
import qualified Bool.TermSet as TermSet

-- | Return the most frequent 'Literal' from the 'TermSet'.
-- The first argument removes the 'Literal's in the 'Term'
-- from the selection.
mostFreqLiteral :: Term -- ^ Literals to filter out
                -> TermSet
                -> Literal
mostFreqLiteral t =
    maxLiteral . rmLiterals t . countTermSet
  where

    -- For every literal in the term, increment the corresponding
    -- count in the map.
    countTerm :: Map Literal Int -> Term -> Map Literal Int
    countTerm = Term.fold
        (flip $ Map.alter (return . succ . fromMaybe 0))

    -- Run countTerm for every term in the termset.
    countTermSet :: TermSet -> Map Literal Int
    countTermSet = TermSet.fold countTerm mempty

    -- Return the literal with the highest count.
    maxLiteral :: Map Literal Int -> Literal
    maxLiteral =
        fst
        . maximumBy (\(_, a) (_, b) -> compare a b)
        . Map.toList

    -- Remove every literal in the term from the map
    rmLiterals :: Term -> Map Literal Int -> Map Literal Int
    rmLiterals = flip $ Term.fold (flip Map.delete)

-- Build a 'TermSet' which covers the entire ON-Set without
-- intersect the OFF-Set.
cdSearch :: TermSet -- ^ ON-Set
         -> TermSet -- ^ OFF-Set
         -> TermSet
cdSearch on off
    | on  == TermSet.empty = TermSet.empty
    | off == TermSet.empty = on
    | otherwise =
        {-TermSet.singleton newTerm-}
        {-TermSet.notCovered newTerm on-}
        TermSet.insert (newTerm)
        $ cdSearch (on TermSet.\\ newTermSet) off
  where

    -- Search for the most frequent literal in the
    -- ON-Set that doesn't intesect the OFF-Set.
    searchTerm :: Term -> TermSet -> (Term, TermSet)
    searchTerm t ts
        | nt `intersects` off = searchTerm nt cts
        | otherwise           = (nt, cts)
      where
        nt = Term.insert (mostFreqLiteral t ts) t
        cts = TermSet.covered nt ts

    (newTerm, newTermSet) = searchTerm Term.empty on
