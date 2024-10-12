module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: Num a => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc xx init ka ks = aux ((Add init):xx) ka ks
	where aux ((Add i):xs) ka ks = aux xs (\v -> ka (v + i)) ks
	      aux ((Sub i):xs) ka ks = aux xs ka (\v -> ks (v - i))
	      aux [] ka ks = ks (ka 0)

