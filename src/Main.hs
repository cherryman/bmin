import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List          as List
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        (All (..), getAll, mappend, mempty)
import           System.IO          (print)


data XBool         = F | X | T deriving (Show, Eq, Ord)
type Var           = Int
type Literal       = (Var, XBool)
type Term          = IntMap XBool -- Map Vars to XBools
type TermSet       = [Term]
type LiteralCount  = (Literal, Integer)
type LiteralCounts = Map Literal Integer

main :: IO ()
main = print $ cdSearch onset offset

onset :: TermSet
onset = map IntMap.fromList
    [ [(0,T), (1,F), (2,T)]
    , [(0,T), (1,F), (2,F)]
    , [(0,T), (1,T), (2,F)]
    , [(0,F), (1,T), (2,T)]
    , [(0,F), (1,T), (2,F)]
    ]

offset :: TermSet
offset = map IntMap.fromList
    [ [(0,F), (1,F), (2,F)]
    , [(0,T), (1,T), (2,T)]
    ]

cdSearch :: TermSet -> TermSet -> TermSet
cdSearch [] _   = []
cdSearch on off =
    newTerm : cdSearch newOn off
  where
    -- Return the new Term and the covered TermSet
    search :: Term -> TermSet -> (Term, TermSet)
    search t n =
        if nt `intersects` off
            then search nt cn
            else (nt, cn)
      where
            nt :: Term
            nt = addLiteral (nextLiteral t n) t

            cn :: TermSet
            cn = covered nt n

    (newTerm, coveredOn) = search mempty on
    newOn = on List.\\ coveredOn

-- Perform a lookup on a Term. Returns X if element is not present
lookupMt :: Var -> Term -> XBool
lookupMt k m = fromMaybe X $ IntMap.lookup k m

-- Check if a Term t covers a term mt
covers :: Term -> Term -> Bool
covers t mt =
    getAll $
    -- Check if every literal from t covers the corresponding literal in mt
    IntMap.foldlWithKey'
        (\acc k v -> mappend acc $ All $ literalCovers (k, v))
        mempty t
  where
    boolCovers :: XBool -> XBool -> Bool
    boolCovers X _ = True
    boolCovers x y = x == y

    literalCovers :: Literal -> Bool
    literalCovers (k, v) = v `boolCovers` lookupMt k mt

-- Check if Term t intersects TermSet ts, i.e. t covers any literal in ts
intersects :: Term -> TermSet -> Bool
intersects _ []       = False
intersects t (ts:tss) = (t `covers` ts) || intersects t tss

addLiteral :: Literal -> Term -> Term
addLiteral (var, val) = IntMap.insert var val

covered :: Term -> TermSet -> TermSet
covered t = filter (covers t)

termCount :: TermSet -> LiteralCounts
termCount = List.foldl' count Map.empty
  where
    -- Increment the key in the Map. Set to 1 if key doesn't exist
    inc :: Literal -> LiteralCounts -> LiteralCounts
    inc k m =
        if Map.member k m
            then Map.adjust (+1) k m
            else Map.insert k 1 m

    -- Update the counts in Map m from Term mt using inc
    count :: LiteralCounts -> Term -> LiteralCounts
    count = IntMap.foldlWithKey' (\acc k v -> inc (k, v) acc)

pickLiteral :: LiteralCounts -> Literal
pickLiteral =
    fst
    . Map.foldlWithKey'
        (\acc k v -> max' acc (k, v)) ((0,X), 0)
  where
    max' :: LiteralCount -> LiteralCount -> LiteralCount
    max' l0@(_, c0) l1@(_, c1)
        | c0 > c1   = l0
        | otherwise = l1

nextLiteral :: Term -> TermSet -> Literal
nextLiteral t =
    pickLiteral . delKeys t . termCount
  where
    delKeys :: Term -> LiteralCounts -> LiteralCounts
    delKeys terms lc =
        IntMap.foldlWithKey' (\acc k v -> Map.delete (k, v) acc) lc terms
