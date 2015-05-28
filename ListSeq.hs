import Seq
import Par

instance Seq [] where
    emptyS      = []
    singletonS  = singletonL
    lengthS     = length
    nthS        = nthL
    tabulateS   = tabulateL
    mapS        = map
    filterS     = filter
    appendS     = (++)
    takeS       = takeL
    dropS       = dropL
    showtS      = showtL
    showlS      = showlL
    joinS       = joinL
    reduceS     = reduceL
    scanS       = scanL
    fromList    = id
   
    
singletonL :: a -> [a]
singletonL x = [x]

nthL :: [a] -> Int -> a 
nthL (x:xs) n =  if n == 0 
                then x 
                else nthL xs (n-1)
                
tabulateL  :: (Int -> a) -> Int -> [a]
tabulateL f 0 = []
tabulateL f 1 = [f 0]
tabulateL f n = (tabulateL f (n-1)) ++ [f (n-1)]

takeL :: [a] -> Int -> [a]
takeL l n = take n l

dropL :: [a] -> Int -> [a]
dropL l n = drop n l

showtL :: [a] -> TreeView a [a]
showtL []   = EMPTY
showtL [x]  = ELT x
showtL xs   = NODE  (take ((length xs) `div` 2) (xs))
                    (drop ((length xs) `div` 2) (xs))

showlL :: [a] -> ListView a [a]
showlL [] = NIL
showlL (x:xs) = CONS x xs

joinL :: [[a]] -> [a]
joinL [] = []
joinL (x:xs) = x ++ joinL xs

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f e []  = e
reduceL f e [x] = f e x 
reduceL f e xs  = let (lReduced, rReduced) = reduceL f e left ||| reduceL f e right
                  in f lReduced rReduced 
                    where left  = tvl (showtL xs)
                          right = tvr (showtL xs)


scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f e []  = ([], e)
scanL f e [x] = ([e], f e x)
scanL f e xs  = let (lScan, rScan) = scanL f e left ||| scanL f e right 
                in  let r = f (snd lScan) (snd rScan) 
                    in  ((fst lScan) , r)
                             where left  = tvl (showtL xs)
                                   right = tvr (showtL xs)

