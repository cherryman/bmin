module Algorithm.Boom
    ( boom
    ) where

import           Algorithm      (cdSearch)
import           Algorithm.Impl (expand)
import           Bool.Function  (Function (..))
import           Bool.TermSet   (TermSet)

-- TODO: boom :: FnGroup -> FnGroup
boom :: Function -> TermSet
boom (Function on off) =
    (`expand` off) . (`cdSearch` off) $ on
