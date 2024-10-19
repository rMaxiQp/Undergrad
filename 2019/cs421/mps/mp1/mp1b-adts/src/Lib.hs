--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons (x:xs) =  Cons x (list2cons xs)
list2cons [] = Nil

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons a b) = a : cons2list b

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (MultExp x) = foldr (*) 1 (map eval x)
eval (PlusExp x) = foldr (+) 0 (map eval x)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' a = foldr ( Cons ) Nil a

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
                | Leaf
                deriving (Show)
--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree (Node a b c) = a + sumTree b + sumTree c
sumTree Leaf = 0

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer 
             | BoolVal Bool
             | StrVal String
             | ExnVal String
             deriving (Show)
--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp f (IntVal a) (IntVal b) = IntVal (f a b)
liftIntOp f _ _ = ExnVal "not an IntVal!"
