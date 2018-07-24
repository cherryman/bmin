module Algorithm
    (
      countLiterals
    , mostFreqLiterals
    , cdSearch
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

import           Bool            (Literal (..), Variable)
import           Bool.Term       (Term)
import qualified Bool.Term       as Term
import           Bool.TermSet    (TermSet, intersects)
import qualified Bool.TermSet    as TermSet

-- | Return a count of each 'Literal' in the 'TermSet'.
countLiterals :: TermSet -> Map Literal Int
countLiterals =
    TermSet.fold countTerm mempty
  where
    countTerm :: Map Literal Int -> Term -> Map Literal Int
    countTerm = Term.fold
        (flip $ Map.alter (return . succ . fromMaybe 0))

-- | Return the most frequent 'Literal' from the 'TermSet'.
-- The first argument removes the 'Literal's in the 'Term'
-- from the selection.
mostFreqLiterals :: [Variable] -- ^ 'Variable's to filter out
                 -> TermSet
                 -> [Literal]
mostFreqLiterals vars ts =
    maxLiterals
  where
    -- Remove every 'Variable' in the list from the 'Term'
    rmVars :: [Variable] -> Term -> Term
    rmVars []     = id
    rmVars (v:vs) = rmVars vs . Term.delVar v

    literalCounts :: Map Literal Int
    literalCounts = countLiterals $ TermSet.map (rmVars vars) ts

    highestCount :: Int
    highestCount = maximum literalCounts

    -- Return the literal with the highest count.
    maxLiterals :: [Literal]
    maxLiterals =
        map fst                       -- extract the literals
        . Map.toList                  -- we want a list of literals
        . Map.filter (==highestCount) -- only keep the highest count
        $ literalCounts

-- Build a 'TermSet' which covers the entire ON-Set without
-- intersect the OFF-Set.
cdSearch :: TermSet -- ^ ON-Set
         -> TermSet -- ^ OFF-Set
         -> TermSet
cdSearch on off
    | on  == TermSet.empty = TermSet.empty
    | off == TermSet.empty = on
    | otherwise =
        TermSet.insert newTerm (cdSearch newOn off)
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
