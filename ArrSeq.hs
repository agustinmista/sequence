module ArrSeq where

import Arr
import Seq
import Par

instance Seq Arr where
    emptyS      = Arr.fromList []
    singletonS  = singletonA
    lengthS     = Arr.length
    nthS        = (!)
    tabulateS   = Arr.tabulate
    mapS        = mapA
    filterS     = filterA
    appendS     = appendA
    takeS       = takeA
    dropS       = dropA
    showtS      = showtA
    showlS      = showlA
    joinS       = Arr.flatten
    reduceS     = reduceA
    scanS       = scanA
    fromList    = Arr.fromList
    
singletonA :: a -> Arr a
singletonA x = Arr.fromList [x]

mapA :: (a -> b) -> Arr a -> Arr b
mapA f a = Arr.tabulate (\x->(f (a!x))) (Arr.length a)

filterA :: (a -> Bool) -> Arr a -> Arr a
filterA p a	| size == 0 = emptyS
            | size == 1 = if (p (a!0)) then singletonA (a!0) else emptyS
            | otherwise = appendA filterL filterR
                where size = Arr.length a
                      (filterL, filterR) = (filterA p (takeA a (size `div` 2))) ||| (filterA p (dropA a (size `div` 2)))


appendA :: Arr a -> Arr a -> Arr a
appendA a b = Arr.tabulate (\x-> if x < sa then a!x else b!(x-sa)) (sa + sb)
              where sa = Arr.length a   
                    sb = Arr.length b

takeA :: Arr a -> Int -> Arr a
takeA a s = Arr.tabulate (\x->(a!x)) s

dropA :: Arr a -> Int -> Arr a
dropA a s = Arr.tabulate (\x->(a!(x + s))) (Arr.length a - s)

showtA :: Arr a -> TreeView a (Arr a)
showtA a | size == 0    = EMPTY
         | size == 1    = ELT (a!0)
         | otherwise    = NODE (takeA a (size `div` 2)) (dropA a (size `div` 2)) 
            where size = Arr.length a
            
showlA :: A.Arr a -> ListView a (A.Arr a)
showlA a | A.length a == 0  = NIL
         | otherwise        = CONS (a!0) (dropA a 1)
showlA :: Arr a -> ListView a (Arr a)
showlA a | Arr.length a == 0  = NIL
         | otherwise          = CONS (a!0) (dropA a 1)

reduceA :: (a -> a -> a) -> a -> Arr a -> a
reduceA f e a | size == 0   = e
              | size == 1   = f e (a!0)
              | otherwise   = reduceA f e (contract f a)
                where size = Arr.length a

scanA :: (a -> a -> a) -> a -> Arr a -> (Arr a, a)
scanA f e a | size == 0     = (emptyS, e)
            | size == 1     = (singletonA e, f e (a!0))
            | otherwise     = (combine f a partial, reduced)
                where size = Arr.length a
                      (partial, reduced) = scanA f e (contract f a)


contract :: (a -> a -> a) -> Arr a -> Arr a
contract f a | size == 1    = a
             | even size    = tabulate (\x->f (a!(2*x)) (a!(2*x+1))) half
             | odd  size    = tabulate (\x->if x == half then (a!(2*x)) else f (a!(2*x)) (a!(2*x+1))) (half+1)
                    where size = Arr.length a
                          half = size `div` 2
                          
            
combine :: (a -> a -> a) -> Arr a -> Arr a -> Arr a
combine f a partial = tabulate aux size
                    where size   = Arr.length a
                          aux x  = if even x 
                                    then (partial!(x `div` 2)) 
                                    else f (partial!(x `div` 2)) (a!(x-1))