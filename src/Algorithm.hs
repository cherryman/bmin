module Algorithm
    (
      countLiterals
    , mostFreqLiterals
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
