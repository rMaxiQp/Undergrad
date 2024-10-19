module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case (H.lookup s env) of 
        Just s' -> s'
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (H.lookup op intOps) of
        Just f -> if op == "/" && v2 == IntVal 0 
                  then ExnVal "Division by 0" 
                  else liftIntOp f v1 v2 
        Nothing -> ExnVal "No matching operator"

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (H.lookup op boolOps) of 
        Just f -> liftBoolOp f v1 v2
        Nothing -> ExnVal "No matching operator"

eval (CompOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
    in case (H.lookup op compOps) of
        Just f -> liftCompOp f v1 v2
        Nothing -> ExnVal "No matching operator"

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    case (eval e1 env) of 
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env =
    let v1 = eval e1 env
    in case v1 of
    CloVal params body clenv -> 
        let v2 = map (\v -> eval v env) args 
        in eval body (foldr (\(k,v) env' -> H.insert k v env') clenv (zip params v2))
    _ -> ExnVal "Apply to non-closure"


--- ### Let Expressions

eval (LetExp pairs body) env =
    let lets = map (\(a,b) -> (a, eval b env)) pairs
    in eval body (H.union (H.fromList lets) env)

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, H.insert var (eval e env) env)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = --(s ++ ss, penvs, envs)
--   where (s, penv', env') = exec x penv env
--         (ss, penvs, envs) = exec (SeqStmt xs) penv' env'
    let (s, penv', env') = exec x penv env
        (ss, penvs, envs) = exec (SeqStmt xs) penv' env'
    in (s ++ ss, penvs, envs)
--- ### If Statements

exec (IfStmt e1 s1 s2) penv env =
    let v1 = eval e1 env
    in case (v1) of 
    BoolVal True -> exec s1 penv env
    BoolVal False -> exec s2 penv env
    _ -> ((show $ ExnVal "Condition is not a Bool"), penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = ("", H.insert name p penv, env)

exec (CallStmt name args) penv env = 
    case (H.lookup name penv) of
        Just (ProcedureStmt name' args' body') -> 
            let v1 = map (\x -> eval x env) args
                v2 = zip args' v1
            in exec body' penv (foldr (\(k,v) e -> H.insert k v e) env v2)
        _ -> ("Procedure " ++ name ++ " undefined", penv, env)
