{-# LANGUAGE FlexibleContexts #-}

module Scheme.Runtime where

import Scheme.Core
import Scheme.Parse
import Scheme.Eval

import qualified Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable

--- ### Helper functions for lifting and lowering

lowerBool :: Val -> Bool
lowerBool (Boolean False) = False
lowerBool _ = True

lowerInt :: Val -> EvalState Int
lowerInt (Number i) = return i
lowerInt v = throwError $ TypeError v

lowerList :: Val -> EvalState [Val]
lowerList (List xx) = return xx
lowerList v = throwError $ TypeError v

liftIntVargOp :: (Int -> Int -> Int) -> Int -> Val
liftIntVargOp f c = PrimFunc p where
  p [] = return $ Number c
  p [x] = Number . f c <$> lowerInt x
  p xx = Number . foldl1 f <$> mapM lowerInt xx

liftBoolVargOp :: ([Bool] -> Bool) -> Val
liftBoolVargOp f = PrimFunc $ return . Boolean . f . map lowerBool

liftIntBinOp :: (Int -> Int -> Int) -> Val
-- liftIntBinOp f = PrimFunc p where
--   p [Number x, Number y] = return $ Number $ f x y
--   p v = throwError $ UnexpectedArgs v
liftIntBinOp f = PrimFunc p where
  p [x, y] = do
    a <- lowerInt x
    b <- lowerInt y
    return $ Number $ f a b
  p v = throwError $ UnexpectedArgs v

liftIntUnaryOp :: (Int -> Int) -> Val
liftIntUnaryOp f = PrimFunc p where
  p [Number x] = return $ Number $ f x
  p v = throwError $ UnexpectedArgs v 

liftBoolUnaryOp :: (Bool -> Bool) -> Val
liftBoolUnaryOp f = PrimFunc p where
  p [Boolean False] = return $ Boolean $ f False
  p [_] = return $ Boolean $ f True
  p v = throwError $ UnexpectedArgs v

liftCompOp :: (Int -> Int -> Bool) -> Val
-- liftCompOp f = PrimFunc p where
--   p [] = return $ Boolean True
--   p xx = mapM lowerInt xx >>= \nums ->
--     return . Boolean . and . map (uncurry f) $ zip nums (tail nums)
liftCompOp f = PrimFunc p where
  p [] = return $ Boolean True
  p [x] = return $ Boolean True
  p xs = 
    Boolean . aux <$> (mapM lowerInt xs) where
      aux [a, b] = f a b
      aux (a:b:c) = (f a b) && (aux (b:c))

--- ### Primtive operations

-- Primitive function `car`
car :: [Val] -> EvalState Val
car [List (x:xs)] = return x
car [DottedList (x:xs) y] = return x
car v = throwError $ UnexpectedArgs v

-- Primitive function `cdr`
cdr :: [Val] -> EvalState Val
-- cdr [List (_:xs)] = return $ List xs
-- cdr [DottedList [_] y] = return y
-- cdr [DottedList (_:xs) y] = return $ DottedList xs y
-- cdr v = throwError $ UnexpectedArgs v 
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (x:xs) rest] = return $ DottedList xs rest
cdr v = throwError $ UnexpectedArgs v

-- Primitive function `cons`
cons :: [Val] -> EvalState Val
cons [x, y] = return $ DottedList [x] y
cons v = throwError $ UnexpectedArgs v

-- Primitive function `list`
list :: [Val] -> EvalState Val
list [] = return $ List []
list xx = return . flattenList $ List xx

-- Primitive function `append`
append :: [Val] -> EvalState Val
append [] = return $ List []
append [x] = return x
append vv = foldlM append' (List []) (map flattenList vv) where
  append' (List []) x = return x
  append' (List xs) (List ys) = return $ List (xs ++ ys)
  append' (List xs) (DottedList ys y) = return $ DottedList (xs ++ ys) y
  append' _ acc = throwError $ TypeError acc

-- Primitive function `apply`
-- It applies a function to a list of parameters
applyPrim :: [Val] -> EvalState Val
-- applyPrim [f, args] = case flattenList args of
--   List xx -> apply f xx
--   v -> throwError $ TypeError v
-- applyPrim v = throwError $ UnexpectedArgs v
applyPrim [f, List xx] = apply f xx 

-- Primitive function `eval`
-- It evaluates the single argument as an expression
-- All you have to do is to check the number of arguments and
-- feed the single argument to the evaluator!
evalPrim :: [Val] -> EvalState Val
-- evalPrim [e] = eval e
-- evalPrim v = throwError $ UnexpectedArgs v
evalPrim [xx] = eval xx 

-- Primitive function `=`, throwing type error for mismatch
-- `=` is a comparison operator for numbers and booleans
equalSign :: [Val] -> EvalState Val
-- equalSign [] = return $ Boolean True
-- equalSign [x] = return $ Boolean True
-- equalSign l@(x:xs) = equalSignTypeValid l >> equalSignVal l
-- equalSignTypeValid :: [Val] -> EvalState Val
-- equalSignTypeValid [] = return $ Boolean True
-- equalSignTypeValid [x] = return $ Boolean True
-- equalSignTypeValid l@(x:xs) =
--   let same_type (a,b) = case (a,b) of
--         ((Number _),(Number _)) -> return $ Boolean True
--         ((Boolean _),(Boolean _)) -> return $ Boolean True
--         (x,y) -> throwError $ TypeError y
--       pairs = zip l (tail l)
--       check_pairs [] = return $ Boolean True
--       check_pairs ((a,b):xs) = same_type (a,b) >> check_pairs xs
--   in check_pairs pairs
-- equalSignVal :: [Val] -> EvalState Val
-- equalSignVal [] = return $ Boolean True
-- equalSignVal [x] = return $ Boolean True
-- equalSignVal (x:xs) = Boolean <$> foldlM (equal' x) True xs where
--   equal' _ False _ = return False
--   equal' (Number a) _ (Number b) = return $ a == b
--   equal' (Boolean a) _ (Boolean b) = return $ a == b
--   equal' y _ _ = throwError $ TypeError y
equalSign [] = return $ Boolean True
equalSign [x] = return $ Boolean True
equalSign xx = aux (zip xx (tail xx)) True where
  aux [] flag = return $ Boolean flag
  aux ((a, b):rest) flag =
    case (a, b) of
      (Number i, Number j) -> aux rest ((i == j) && flag)
      (Boolean i, Boolean j) -> aux rest ((i == j) && flag)
      (_, _) -> throwError $ TypeError a

-- Primitive function `eq?`, not throwing any error
-- `eq?` is a comparison operator for atom values (numbers, booleans, and symbols)
-- Returns `#f` on type mismatch or unsupported types (functions etc)
eq :: [Val] -> EvalState Val
-- eq [] = return $ Boolean True
-- eq (x:xs) = return $ Boolean $ foldl (eq' x) True xs where
--   eq' _ False _ = False
--   eq' (Number a) _ (Number b) = a == b
--   eq' (Boolean a) _ (Boolean b) = a == b
--   eq' (Symbol a) _ (Symbol b) = a == b
--   eq' _ _ _ = False
eq [] = return $ Boolean True
eq [x] = return $ Boolean True
eq xx = return $ Boolean $ all aux (zip xx (tail xx)) where
  aux (a, b) = 
    case (a, b) of 
      (Number i, Number j) -> i == j
      (Boolean i, Boolean j) -> i == j
      (Symbol i, Symbol j) -> i == j
      (_, _) -> False
-- Primitive function `list?` predicate
-- `(list? arg)` determines whether `arg` is a non-dotted list
-- or an empty list (null)
isList :: [Val] -> EvalState Val
isList [v] =
  return . Boolean $ case flattenList v of 
    List _ -> True
    _ -> False
isList vv = throwError $ UnexpectedArgs vv

-- Primitive function `symbol?` predicate
isSymbol :: [Val] -> EvalState Val
isSymbol [Symbol _] = return $ Boolean True
isSymbol [_] = return $ Boolean False
isSymbol v = throwError $ UnexpectedArgs v

-- Primitive function `pair?` predicate
-- Any `List` or `DottedList` is a pair
isPair :: [Val] -> EvalState Val
-- isPair [v] =
--   return . Boolean $ case flattenList v of
--     List (_:_) -> True
--     DottedList _ _ -> True
--     _ -> False
-- isPair vv = throwError $ UnexpectedArgs vv
isPair [List x] = case x of
  [] -> return $ Boolean False
  _ -> return $ Boolean True
isPair [DottedList _ _ ]= return $ Boolean True
isPair [_] = return $ Boolean False
isPair v = throwError $ UnexpectedArgs v

-- Primitive function `number?` predicate
isNumber :: [Val] -> EvalState Val
isNumber [Number _] = return $ Boolean True
isNumber [_] = return $ Boolean False
isNumber v = throwError $ UnexpectedArgs v

-- Primitive function `boolean?` predicate
isBoolean :: [Val] -> EvalState Val
isBoolean [Boolean _] = return $ Boolean True
isBoolean [_] = return $ Boolean False
isBoolean v = throwError $ UnexpectedArgs v

-- Primitive function `null?` predicate
-- An empty list or its *equivalent* value is null
-- Note: Think about what's equivalent
isNull :: [Val] -> EvalState Val
-- isNull [v] =
--   return . Boolean $ case flattenList v of
--     List [] -> True
--     _ -> False
-- isNull vv = throwError $ UnexpectedArgs vv
isNull [x] = case x of
  List [] -> return $ Boolean True
  _ -> return $ Boolean False
isNull v = throwError $ UnexpectedArgs v

--- ### Runtime

runtime :: Env
runtime = H.fromList [ ("+", liftIntVargOp (+) 0)
                     , ("-", liftIntVargOp (-) 0)
                     , ("*", liftIntVargOp (*) 1)
                     , ("/", liftIntVargOp (div) 1)
                     , (">", liftCompOp (>))
                     , ("<", liftCompOp (<))
                     , (">=", liftCompOp (>=))
                     , ("<=", liftCompOp (<=))
                     , ("=", PrimFunc equalSign)
                     , ("and", liftBoolVargOp and)
                     , ("or", liftBoolVargOp or)
                     , ("cons", PrimFunc cons)
                     , ("append", PrimFunc append)
                     , ("symbol?", PrimFunc isSymbol)
                     , ("list?", PrimFunc isList)
                     , ("eq?", PrimFunc eq)
                     , ("car", PrimFunc car)
                     , ("cdr", PrimFunc cdr)
                     , ("pair?", PrimFunc isPair)
                     , ("null?", PrimFunc isNull)
                     , ("number?", PrimFunc isNumber)
                     , ("boolean?", PrimFunc isBoolean)
                     , ("apply", PrimFunc applyPrim)
                     , ("eval", PrimFunc evalPrim)
                     , ("not", liftBoolUnaryOp not)
                     , ("list", PrimFunc list)
                     , ("modulo", liftIntBinOp mod)
                     , ("abs", liftIntUnaryOp abs)
                     ]
