module Lib
    (chop) where


-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop [] = []
chop (x:y:xs)
	| x > 0 = (x-1) : (y + (length (y:xs)) : xs)
	| otherwise = x : (chop (y:xs))
chop (x:xs)  
	| x > 0 = [x - 1]
	| otherwise = [0]
	

