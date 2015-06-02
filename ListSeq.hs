module ListSeq where

import Seq
import Par

instance Seq [] where
    emptyS      = []
    singletonS  = singletonL
    lengthS     = length
    nthS        = nthL
    tabulateS   = tabulateL
    mapS        = map
    filterS     = filterL
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
nthL (x:xs) n | n == 0    = x
              | otherwise = nthL xs (n-1) 
                
tabulateL  :: (Int -> a) -> Int -> [a]
tabulateL f 0 = []
tabulateL f 1 = [f 0]
tabulateL f n = (tabulateL f (n-1)) ++ [f (n-1)]

filterL :: (a -> Bool) -> [a] -> [a]
filterL f [] = []
filterL f (x:xs) = if b then (x:r) else r
                        where (b, r) = f x ||| filterL f xs
                        
takeL :: [a] -> Int -> [a]
takeL l n = Prelude.take n l

dropL :: [a] -> Int -> [a]
dropL l n = Prelude.drop n l

showtL :: [a] -> TreeView a [a]
showtL []   = EMPTY
showtL [x]  = ELT x
showtL xs   = NODE (take (size `div` 2) (xs)) (drop (size `div` 2) (xs)) 
                where size = length xs

showlL :: [a] -> ListView a [a]
showlL [] = NIL
showlL (x:xs) = CONS x xs

joinL :: [[a]] -> [a]
joinL [] = []
joinL (x:xs) = x ++ joinL xs

reduceL :: (a -> a -> a) -> a -> [a] -> a
reduceL f e []       = e
reduceL f e [x]      = f e x 
reduceL f e xs = reduceL f e (contract f xs)

scanL :: (a -> a -> a) -> a -> [a] -> ([a], a)
scanL f e []  = ([], e)
scanL f e [x] = ([e], f e x)
scanL f e xs  = (combine True f xs partial, reduced)
                  where (partial, reduced) = scanL f e (contract f xs)

contract :: (a -> a -> a) -> [a] -> [a]
contract f []  = []
contract f [x] = [x]
contract f (x:y:xs) = (f x y) : (contract f xs)

combine :: Bool -> (a -> a -> a) -> [a] -> [a] -> [a]
combine _ _ [] _ = []
combine True  f s@(_:_)   c@(x:_)   = x : (combine False f s c)
combine False f [_]       (_:_)     = []
combine False f (x:_:xs)  (x':xs')  = (f x' x) : (combine True f xs xs')