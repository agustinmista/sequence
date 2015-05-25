import Seq
import Par

instance Seq [] where
    emptyS      = []
    singletonS  = undefined
    lengthS     = length
    nthS        = undefined
    tabulateS   = undefined
    mapS        = map
    filterS     = filter
    appendS     = undefined
    takeS       = undefined
    dropS       = drop
    showtS      = undefined
    showlS      = undefined
    joinS       = undefined
    reduceS     = undefined
    scanS       = undefined
    fromList    = undefined
    
    

s = emptyS :: [] Int