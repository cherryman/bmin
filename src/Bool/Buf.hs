module Bool.Buf
    (
      FnBuf
    , Buf
    , fromFnGroup
    ) where

import           Bool.Function (FnGroup, Function)
import           Bool.TermSet  (TermSet)
import qualified Bool.TermSet  as TermSet

data FnBuf = FnBuf
    { fn        :: Function
    , implBuf   :: TermSet
    , expandBuf :: TermSet
    }

data Buf = Buf
    { fns       :: [FnBuf]
    , reduceBuf :: TermSet
    }

fromFn :: Function -> FnBuf
fromFn f = FnBuf f TermSet.empty TermSet.empty

fromFnGroup :: FnGroup -> Buf
fromFnGroup f = Buf (map fromFn f) TermSet.empty
