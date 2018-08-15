module Bool.Buf
    (
      FnBuf(..)
    , Buf(..)
    , fromFn
    , fromFnGroup
    ) where

import           Bool.Function (FnGroup, Function)
import           Bool.TermSet  (TermSet)
import qualified Bool.TermSet  as TermSet

data FnBuf = FnBuf
    { bufFn     :: Function
    , implBuf   :: TermSet
    , expandBuf :: TermSet
    }
    deriving (Show, Eq)

data Buf = Buf
    { bufFns    :: [FnBuf]
    , reduceBuf :: TermSet
    }

fromFn :: Function -> FnBuf
fromFn f = FnBuf f TermSet.empty TermSet.empty

fromFnGroup :: FnGroup -> Buf
fromFnGroup f = Buf (map fromFn f) TermSet.empty
