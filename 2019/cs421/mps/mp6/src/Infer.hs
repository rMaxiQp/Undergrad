
module Infer where

import Common

import Data.Map.Strict as H (Map, insert, lookup, empty)

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> FVState MonoTy
freshInst (_, tau@(TyVar _)) = return tau
freshInst ([], tau) = return tau
freshInst ((x:xs), tau) = do
  v <- freshTVar
  let newtau = liftMonoTy (substInit x $ TyVar v) tau
  freshInst (xs, newtau)

freshInstFV :: FVState PolyTy -> FVState MonoTy
freshInstFV s = s >>= (\tau -> freshInst tau)

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = 
  let 
    vars = freeVarsMonoTy tau
  in i `elem` vars 


  {- question 3: unification -}

unify :: Exp -> [(MonoTy, MonoTy)] -> FVState SubstEnv
unify exp [] = return H.empty
unify exp (x:xs) = aux x
    where 
      -- Delete rule --
      aux x@(s, t)
        | s == t = unify exp xs
      
      -- Orient rule --
      aux (s@(TyConst _ _), t@(TyVar _)) = unify exp ((t,s):xs)
      
      -- Decompose rule --
      aux ((TyConst "pair" [s1, s2]), (TyConst "pair" [t1, t2])) = unify exp (xs ++ [(s1, t1), (s2, t2)])
      aux ((TyConst "fun" [s1, s2]), (TyConst "fun" [t1, t2])) = unify exp (xs ++ [(s1, t1), (s2, t2)])
      aux ((TyConst "list" [s]), (TyConst "list" [t])) = unify exp (xs ++ [(s, t)])

      -- Eliminate rule --
      aux (s@(TyVar v), t) 
        | occurs v t == False = do
          let sub = substInit v t
          vv <- unify exp (map (\(a, b) -> (liftMonoTy sub a, liftMonoTy sub b)) xs)
          return (H.insert v (liftMonoTy vv t) vv)
      
      -- UnifError --
      aux _ = throwError $ UnifError exp

  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> MonoTy -> FVState SubstEnv
infer env e@(ConstExp c) tau = do
  const <- constTySig c
  ins <- freshInst const
  unify e [(tau, ins)]

infer env e@(VarExp x) tau = 
  case (H.lookup x env) of
    Just ex -> do
      ins <- freshInst ex
      unify e [(tau, ins)]
    Nothing -> throwError $ LookupError x

infer env e@(LetExp x e1 e2) tau = do
  ta <- freshTau
  s1 <- infer env e1 ta
  let g = gen (liftEnv s1 env) (liftMonoTy s1 ta) 
  s2 <- infer (H.insert x g (liftEnv s1 env)) e2 (liftMonoTy s1 tau)
  return $ substCompose s2 s1

infer env e@(MonOpExp m e') tau = do
  ta <- freshTau
  s1 <- infer env e' ta
  m1 <- monopTySig m
  ins <- freshInst m1
  uni <- unify e [(liftMonoTy s1 (funTy ta tau), ins)]
  return $ substCompose s1 uni

infer env e@(BinOpExp b e1 e2) tau = do
  ta1 <- freshTau
  ta2 <- freshTau
  s1 <- infer env e1 ta1
  s2 <- infer (liftEnv s1 env) e2 ta2
  bi <- binopTySig b
  ins <- freshInst bi
  uni <- unify e [(liftMonoTy (substCompose s2 s1) (funTy ta1 (funTy ta2 tau)), ins)]
  return $ substCompose (substCompose uni s2) s1

infer env e@(IfExp e1 e2 e3) tau = do
  s1 <- infer env e1 boolTy
  s2 <- infer (liftEnv s1 env) e2 (liftMonoTy s1 tau)
  let ss = substCompose s2 s1
  s3 <- infer (liftEnv ss env) e3 (liftMonoTy ss tau)
  return $ substCompose (substCompose s3 s2) s1

infer env e@(FunExp x e') tau = do
  ta1 <- freshTau
  ta2 <- freshTau
  s1 <- infer (H.insert x (quantifyMonoTy ta1) env) e' ta2
  uni <- unify e [(liftMonoTy s1 tau, liftMonoTy s1 (funTy ta1 ta2))]
  return $ substCompose uni s1

infer env e@(AppExp e1 e2) tau = do
  ta <- freshTau
  s1 <- infer env e1 (funTy ta tau)
  s2 <- infer (liftEnv s1 env) e2 (liftMonoTy s1 ta)
  return $ substCompose s2 s1

infer env e@(LetRecExp f x e1 e2) tau = do 
  ta1 <- freshTau
  ta2 <- freshTau
  let m1 = H.insert f (quantifyMonoTy $ funTy ta1 ta2) env
  s1 <- infer (H.insert x (quantifyMonoTy ta1) m1) e1 ta2
  let m2 = liftMonoTy s1 (funTy ta1 ta2)
  s2 <- infer (H.insert f (gen (liftEnv s1 env) m2) (liftEnv s1 env)) e2 (liftMonoTy s1 tau)
  return $ substCompose s2 s1

inferInit :: TypeEnv -> Exp -> FVState MonoTy
inferInit env e = do
  tau <- freshTau
  sEnv <- infer env e tau
  return (liftMonoTy sEnv tau)

inferDec :: TypeEnv -> Dec -> FVState (TypeEnv, MonoTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x (quantifyMonoTy tau) env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f (quantifyMonoTy tau) env, tau)