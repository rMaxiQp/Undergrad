{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (List [c, e]) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
-- Examples:
--   getListOf2 (List (Number 1) (Symbol "x"))
--     ==> ((Number 1), (Symbol "x"))
--   getListOf2 (List (Number 1))
--     ==> Not a list of two elements: (1)
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (List [c, e]) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
-- eval (Symbol sym) = 
--   H.lookup sym `liftM` get >>=
--     \case Just val -> return val
--           _ -> throwError $ UndefSymbolError sym
eval (Symbol sym) = do
  env <- get
  case H.lookup sym env of
    Just val -> return val
    _ -> throwError $ UndefSymbolError sym

-- Dotted list may just be an equivalent representation of List.
-- We simply try to flatten the list. If it's still dotted after
-- flattening, it's an invalid expression.
eval expr@(DottedList _ _) = case flattenList expr of
  DottedList _ _ -> throwError $ InvalidExpression expr
  v -> eval v

-- List evaluates as a form of the following
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(List lst) = evalList $ map flattenList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    evalList [Symbol "quote", e] = return e

    -- unquote (illegal at surface evaluation)
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (List [Symbol "unquote", v]) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (List [Symbol "unquote", v]) = eval v
      evalQuasi n (List ee@[Symbol "quasiquote", _]) = List <$> evalQuasi (n+1) `mapM` ee
      evalQuasi n (List ee@[Symbol "unquote", _]) = List <$> evalQuasi (n-1) `mapM` ee
      evalQuasi n (List xx) = List <$> mapM (evalQuasi n) xx
      evalQuasi n (DottedList xx y) = DottedList <$> mapM (evalQuasi n) xx <*> evalQuasi n y
      evalQuasi _ v = return v

    -- Why comment these out? Because `if` can be defined as a macro!
    -- -- if-then
    -- evalList [Symbol "if", condE, thenE] =
    --   eval condE >>= \c -> if lowerBool c then eval thenE else return Void
    -- -- if-then-else
    -- evalList [Symbol "if", condE, thenE, elseE] =
    --   eval condE >>= \c -> eval $ if lowerBool c then thenE else elseE

    -- cond
    -- evalList (Symbol "cond" : rest) =
    --   mapM getListOf2 rest >>= evalClauses where
    --     -- Empty rest
    --     evalClauses [] = return Void
    --     -- `else` at the end
    --     evalClauses ((Symbol "else", e) : []) = eval e
    --     -- `else` in the middle
    --     evalClauses ((Symbol "else", _) : _) = InvalidSpecialForm "cond"
    --     -- Normal case
    --     evalClauses ((c, e) : cs) =
    --       eval c >>= \case Boolean False -> evalClauses cs
    --                        _ -> eval e
    evalList ((Symbol "cond") : []) = invalidSpecialForm "cond"
    evalList ((Symbol "cond") : rest) = mapM getListOf2 rest >>= aux where
      aux [] = return Void
      aux (x:xs) = do
        case x of
          (Symbol "else", e) ->
            if null xs
              then eval e
              else invalidSpecialForm "cond"
          (xx, e) -> eval xx >>= \a -> case a of
            Boolean False -> aux xs
            _ -> eval e

    -- let
    evalList [Symbol "let", List clauses, body] = do
      env <- get
      v <- mapM getBinding clauses
      mapM (\(a,b) -> modify $ H.insert a b) v
      retval <- eval body
      put env
      return retval

    -- let*
    -- evalList [Symbol "let*", List clauses, body] = do
    --   env <- get
    --   forM_ clauses $ getBinding >=> modify . uncurry H.insert
    --   val <- eval body
    --   put env
    --   return val
    evalList [Symbol "let*", List clauses, body] = aux clauses body where
      aux [] b = do
        env <- get 
        retval <- eval b
        put env
        return retval
      aux (clause : rest) b = do
        env <- get
        (x, y) <- getBinding clause
        modify $ H.insert x y
        r <- aux rest b
        put env
        return r

    -- lambda
    evalList [Symbol "lambda", List args, body] = do
      env <- get
      v <- (\argv -> Func argv body env) <$> mapM getSym args
      return v

    -- define function
    evalList [Symbol "define", List (Symbol fname : args), body] =
      do env <- get
         val <- (\argVal -> Func argVal body env) <$> mapM getSym args
         modify $ H.insert fname val
         return Void

    -- define variable
    -- evalList [Symbol "define", Symbol name, vexpr] = do
    --   modify . (H.insert name) =<< eval vexpr
    --   return Void
    evalList [Symbol "define", Symbol var, body] = do
      val <- eval body
      modify $ H.insert var val
      return Void

    -- define-macro
    -- evalList [Symbol "define-macro", List (Symbol fname : args), body] = do
    --   val <- flip Macro body <$> mapM getSym args
    --   modify $ H.insert fname val
    --   return Void
    evalList [Symbol "define-macro", List (Symbol fname : args), body] = do
      val <- mapM getSym args
      modify $ H.insert fname $ Macro val body
      return Void

    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) = eval fexpr >>= aux where
    --   -- Macro expansion
    --   aux (Macro fmls body) | length fmls == length args = do
    --     -- Save environment
    --     env <- get
    --     -- Insert to env
    --     modify $ H.union (H.fromList (zip fmls args))
    --     -- Expansion using eval 
    --     expanded <- eval body
    --     -- Restore environment
    --     put env
    --     -- Eval expanded body expressions
    --     eval expanded
    --   -- Function application
    --   aux f = mapM eval args >>= apply f
      -- Macro expansion
      aux (Macro fmls body) | length fmls == length args = do
        env <- get                                                       -- Save the environment
        mapM_ (\(a, b) -> modify $ H.insert a b) $ zip fmls args         -- Blind arguments (without evaluating them) to the parameters of the macro and insert them to the environment
        mb <- eval body                                                  -- Evaluate the macro body (i.e. expand the macro body)
        put env                                                          -- Restore the environment
        retval <- eval mb                                                -- Evaluate the expanded form
        return retval
      -- Function application
      aux f = do                                                         -- See apply :: Val -> [Val] -> EvalState Val
        xx <- mapM eval args
        retval <- apply f xx
        return retval 

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
-- Function
apply (Func fmls body cenv) args | length fmls == length args = do
  env <- get                                                             -- Save the environment
  put $ H.union cenv env                                                 -- Insert bindings of the closure environment to the current environment
  mapM (\(a,b) -> modify $ H.insert a b) $ zip fmls args                 -- Bind arguments to the parameters of the function and inser them to the environment
  fb <- eval body                                                        -- Evaluate the function body
  put env                                                                -- Restore the environment we saved
  return fb

-- Primitive
apply (PrimFunc p) args = p args                                         -- Directly apply the primitive function to the argument list

-- Other values are not applicable
apply f args = throwError $ CannotApply f args                           -- throw a diagnostic CannotApply
