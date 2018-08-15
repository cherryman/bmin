module Bool.Function
    (
      Function(..)
    , FnGroup
    , onSets
    , offSets
    ) where

import           Bool.TermSet (TermSet)

data Function = Function
    { onSet  :: TermSet
    , offSet :: TermSet
    }
    deriving (Show, Eq)

type FnGroup = [Function]

onSets :: FnGroup -> [TermSet]
onSets = map onSet

offSets :: FnGroup -> [TermSet]
offSets = map offSet
