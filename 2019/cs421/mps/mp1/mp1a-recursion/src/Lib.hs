--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n (x:xs) 
    | n > 0 = x : mytake (n-1) xs
    | otherwise = []
mytake _ [] = []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n (x:xs) 
    | n > 0 = mydrop (n-1) xs
    | otherwise = (x:xs)
mydrop _ [] = []

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xx = aux xx []
    where aux (x:xs) y = aux xs (x:y)
          aux [] y = y

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] yy = yy
app xx [] = xx
app xx yy = aux (rev xx) yy
    where aux [] y = y
          aux (i:is) y = aux is (i:y) 

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist (x:xs) = (x+1) : inclist xs 
inclist _ = []

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist (x:xs) = x + sumlist xs
sumlist _ = 0

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys
myzip _ _ = []

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs x y = aux (myzip x y)
    where aux ((xx,yy):xs) = (xx + yy) : aux xs
          aux _ = []

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = [1,1..]

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0,1..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : (addpairs fib (tail fib)) 

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n (x:xs)
    | n < x = (n:x:xs)
    | n > x = x : add n xs
    | otherwise = (x: xs)
add n _ = [n]

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union (x:xs) (y:ys)
    | x > y = y : union (x:xs) ys
    | x < y = x : union xs (y:ys)
    | otherwise = x : union xs ys
union _ (y:ys) = (y:ys)
union (x:xs) _ = (x:xs)
union _ _ = []

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect (x:xs) (y:ys)
    | x < y = intersect xs (y:ys)
    | x > y = intersect (x:xs) ys
    | otherwise = x : intersect xs ys
intersect _ _ = []

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset (x:xs) = union (P.map (add x) (powerset xs)) (powerset xs)
powerset [] = [[]]
--powerset (x:xs) = union [ (x:p) | p <- (powerset xs) ] (powerset xs)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' x = P.map (+1) x

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: Num a => [a] -> a
sumlist' x = P.foldr (+) 0 x 
