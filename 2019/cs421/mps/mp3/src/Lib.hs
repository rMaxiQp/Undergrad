--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk x k = factk (x - 1) (\v -> k $ x * v)

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] k1 k2 
    | even x = k1 x
    | otherwise = k2 x
evenoddk (x:xs) k1 k2
    | even x = evenoddk xs (\v -> k1 $ v + x) k2
    | otherwise = evenoddk xs k1 (\v -> k2 $ v + x)

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (IfExp e1 e2 e3) = all isSimple [e1, e2, e3]
isSimple (AppExp _ _) = False
isSimple _ = True

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp x@(IntExp _) k n = (AppExp k x, n)
cpsExp x@(VarExp _) k n = (AppExp k x, n)

--- #### Define `cpsExp` for Application Expressions
cpsExp x@(AppExp f e) k n =
    case isSimple e of
        True -> (AppExp x k, n)
        False -> 
            let
                (v, n') = gensym n
            in cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) n'
--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp f e1 e2) k n =
    let 
        (v1, n1) = gensym n
    in case (isSimple e1, isSimple e2) of
        (True, True) -> (AppExp k $ OpExp f e1 e2, n)
        (True, False) -> cpsExp e2 (LamExp v1 (AppExp k $ OpExp f e1 (VarExp v1))) n1 
        (False, True) -> cpsExp e1 (LamExp v1 (AppExp k $ OpExp f (VarExp v1) e2)) n1
        (False, False) -> 
            let
                (v2, n2) = gensym n1
                (e3, n3) = cpsExp e2 (LamExp v2 (AppExp k $ OpExp f (VarExp v1) (VarExp v2))) n2
            in
                cpsExp e1 (LamExp v1 e3) n3

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n =
    case isSimple e1 of
        True ->
            let
                (ce2, cn2) = cpsExp e2 k n
                (ce3, cn3) = cpsExp e3 k cn2
            in  (IfExp e1 ce2 ce3, cn3)
        False ->
            let
                (v1, n1) = gensym n
                (ce2, cn2) = cpsExp e2 k n1
                (ce3, cn3) = cpsExp e3 k cn2
            in  
                cpsExp e1 (LamExp v1 $ IfExp (VarExp v1) ce2 ce3) cn3

--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f s e) = 
    let (e', _) = cpsExp e (VarExp "k") 0
    in Decl f (s ++ ["k"]) e'
