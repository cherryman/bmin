module Algorithm.Boom
    ( boom
    ) where

import           Algorithm.Impl (cdSearch, expand)
import           Bool.Function  (Function (..))
import           Bool.TermSet   (TermSet)

-- TODO: boom :: FnGroup -> FnGroup
boom :: Function -> TermSet
boom (Function on off) = error "Unimplemented"
    {-(`expand` off) . (`cdSearch` off) $ on-}
