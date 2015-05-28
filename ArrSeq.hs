import qualified Arr as A
import Arr (!)

instance Seq [] where
    emptyS      = A.empty
    singletonS  = undefined
    lengthS     = A.length
    nthS        = (!)
    tabulateS   = A.tabulate
    mapS        = undefined
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