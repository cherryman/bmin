import           Algorithm    (cdSearch)
import           Bool         (Value (..))
import           Bool.TermSet (TermSet)
import qualified Bool.TermSet as TermSet

onset :: TermSet
onset = TermSet.fromPairList
    [ [("0",T), ("1",F), ("2",T)]
    , [("0",T), ("1",F), ("2",F)]
    , [("0",T), ("1",T), ("2",F)]
    , [("0",F), ("1",T), ("2",T)]
    , [("0",F), ("1",T), ("2",F)]
    ]

offset :: TermSet
offset = TermSet.fromPairList
    [ [("0",F), ("1",F), ("2",F)]
    , [("0",T), ("1",T), ("2",T)]
    ]

main :: IO ()
main = print $ cdSearch onset offset
