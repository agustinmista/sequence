{- Implementación del TAD secuencia -}

module Seq where

class Seq s where
   emptyS     :: s a
   singletonS :: a -> s a
   lengthS    :: s a -> Int 
   nthS       :: s a -> Int -> a 
   tabulateS  :: (Int -> a) -> Int -> s a
   mapS       :: (a -> b) -> s a -> s b 
   filterS    :: (a -> Bool) -> s a -> s a 
   appendS    :: s a -> s a -> s a
   takeS      :: s a -> Int -> s a
   dropS      :: s a -> Int -> s a
   showtS     :: s a -> TreeView a (s a)
   showlS     :: s a -> ListView a (s a)
   joinS      :: s (s a) -> s a
   reduceS    :: (a -> a -> a) -> a -> s a -> a
   scanS      :: (a -> a -> a) -> a -> s a -> (s a, a)
   fromList   :: [a] -> s a

data TreeView a t = EMPTY | ELT a | NODE t t
data ListView a t = NIL | CONS a t

instance (Show a, Show t) => Show (TreeView a t) where
    show EMPTY = "EMPTY"
    show (ELT a) = "ELT " ++ show a
    show (NODE t1 t2) = "NODE " ++ show t1 ++ " " ++ show t2
    
instance (Show a, Show t) => Show (ListView a t) where
    show NIL = "NIL"
    show (CONS a t) = "CONS " ++ show a ++ " : " ++ show t
