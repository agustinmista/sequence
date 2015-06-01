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
    filterS     = filterA
    appendS     = appendA
    takeS       = takeA
    dropS       = dropA
    showtS      = showtA
    showlS      = showlA
    joinS       = A.flatten
    reduceS     = undefined
    scanS       = undefined
    fromList    = A.fromList
    
singletonA :: a -> A.Arr a
singletonA x = A.fromList [x]

mapA :: (a -> b) -> A.Arr a -> A.Arr b
mapA f a = A.tabulate (\x->(f (a!x))) (A.length a)

filterA :: (a -> Bool) -> A.Arr a -> A.Arr a
filterA p a	| size == 0 = emptyS
            | size == 1 = if (p (a!0)) then singletonA (a!0) else emptyS
            | otherwise = appendA filterL filterR
				where size = A.length a
				      (filterL, filterR) = (filterA p (takeA a (size `div` 2))) ||| (filterA p (dropA a (size `div` 2)))


appendA :: A.Arr a -> A.Arr a -> A.Arr a
appendA a b = A.tabulate (\x-> if x < sa then a!x else b!(x-sa)) (sa + sb)
              where sa = A.length a   
                    sb = A.length b

takeA :: A.Arr a -> Int -> A.Arr a
takeA a s = A.tabulate (\x->(a!x)) s

dropA :: A.Arr a -> Int -> A.Arr a
dropA a s = A.tabulate (\x->(a!(x + s))) (A.length a - s)

showtA :: A.Arr a -> TreeView a (A.Arr a)
showtA a | size == 0    = EMPTY
         | size == 1    = ELT (a!0)
         | otherwise    = NODE (takeA a (size `div` 2)) (dropA a (size `div` 2)) 
            where size = A.length a
            
showlA :: A.Arr a -> ListView a (A.Arr a)
showlA a | A.length a == 0  = NIL
         | otherwise        = CONS (a!0) (dropA a 1)
