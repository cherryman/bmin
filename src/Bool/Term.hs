module Bool.Term
    (
      Term
    , empty
    , singleton
    , fromList
    , fromPairList
    , varList
    , lookup
    , insert
    , delete
    , delVar
    , member
    , fold
    , covers
    ) where

import           Prelude         hiding (lookup)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe)

import           Bool
    (Literal (..), Value (..), Variable (..), coversValue)


-- | A set of variables with an associated value.
newtype Term = Term
    { unTerm :: Map Variable Value }
    deriving (Eq, Ord)

instance Show Term where
    show (Term t) = show $
        Map.foldlWithKey'
          (\acc k v -> show (Literal k v) : acc)
          [] t

empty :: Term
empty =
    Term Map.empty

singleton :: Literal -> Term
singleton (Literal var val) =
    Term $ Map.singleton var val

-- | Lookup the 'Value' of a 'Variable' in a 'Term'.
lookup :: Variable -> Term -> Value
lookup v =
    fromMaybe X . Map.lookup v . unTerm

-- | Insert a 'Literal' into a 'Term'.
-- A 'Literal' with a value of 'X' deletes the
-- corresponding element as 'lookup' returns 'X'
-- when the 'Variable' isn't a member of the 'Term'.
insert :: Literal -> Term -> Term
insert (Literal var val)
    | val == X  = delVar var --Term . Map.delete var     . unTerm
    | otherwise = Term . Map.insert var val . unTerm

fromList :: [(Variable, Value)] -> Term
fromList = Term . Map.fromList

fromPairList :: [(String, Value)] -> Term
fromPairList =
    fromList . fmap mkVar
  where
    mkVar (var, val) = (Variable var, val)

-- | A list of 'Variable's in the 'Term'.
varList :: Term -> [Variable]
varList = fold (\a (Literal v _) -> v:a) []

-- | Delete a 'Literal' from a 'Term'.
-- Does nothing if the 'Literal' is not
-- present.
delete :: Literal -> Term -> Term
delete l@(Literal var _) =
    Term . Map.updateWithKey del var . unTerm
  where
    -- Return 'Nothing' if the 'Literal' is present
    -- in the 'Term'.
    del :: Variable -> Value -> Maybe Value
    del k v
        | l == Literal k v = Nothing
        | otherwise        = Just v

-- | Delete a 'Variable' from a 'Term'.
delVar :: Variable -> Term -> Term
delVar v = Term . Map.delete v . unTerm

-- | Check if a 'Literal' is a member of a 'Term'
member ::  Literal -> Term -> Bool
member (Literal var val) t =
    lookup var t == val

-- | Fold over a 'Term'.
fold :: (a -> Literal -> a) -> a -> Term -> a
fold f a = Map.foldlWithKey'
    (\acc k v -> f acc $ Literal k v) a . unTerm

-- | Check if a 'Term' covers a second 'Term'.
-- To do so, we check if every 'Literal' in the
-- second 'Term' is covered by the corresponding
-- 'Variable' in the first 'Term'.
covers :: Term -> Term -> Bool
covers t0 = test t0_list
  where
    t0_list :: [(Variable, Value)]
    t0_list = Map.toList $ unTerm t0

    -- Test every literal in the term.
    test :: [(Variable, Value)] -> Term -> Bool
    test [] _         = True
    test ((k,v):ts) t =
        v `coversValue` lookup k t
        && test ts t
