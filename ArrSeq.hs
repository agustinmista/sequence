import qualified Arr as A
import Arr ((!))

import Seq
import Par

instance Seq A.Arr where
    emptyS      = A.fromList []
    singletonS  = singletonA
    lengthS     = A.length
    nthS        = (!)
    tabulateS   = A.tabulate
    mapS        = mapA
    filterS     = undefined
    appendS     = undefined
    takeS       = undefined
    dropS       = undefined
    showtS      = undefined
    showlS      = undefined
    joinS       = undefined
    reduceS     = undefined
    scanS       = undefined
    fromList    = A.fromList
    
singletonA :: a -> Arr a
singletonA x = A.fromList [x]

