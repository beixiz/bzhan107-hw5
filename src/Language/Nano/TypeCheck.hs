{-# LANGUAGE FlexibleInstances, OverloadedStrings, BangPatterns #-}

module Language.Nano.TypeCheck where

import Language.Nano.Types
import Language.Nano.Parser

import qualified Data.List as L
import           Text.Printf (printf)  
import           Control.Exception (throw)

--------------------------------------------------------------------------------
typeOfFile :: FilePath -> IO Type
typeOfFile f = parseFile f >>= typeOfExpr

typeOfString :: String -> IO Type
typeOfString s = typeOfExpr (parseString s)

typeOfExpr :: Expr -> IO Type
typeOfExpr e = do
  let (!st, t) = infer initInferState preludeTypes e
  if (length (stSub st)) < 0 then throw (Error ("count Negative: " ++ show (stCnt st)))
  else return t

--------------------------------------------------------------------------------
-- Problem 1: Warm-up
--------------------------------------------------------------------------------

-- | Things that have free type variables
class HasTVars a where
  freeTVars :: a -> [TVar]

-- | Type variables of a type
instance HasTVars Type where
  freeTVars t     = case t of
    TInt      -> []
    TBool     -> []
    t1 :=> t2 -> if ((freeTVars t1) /= (freeTVars t2)) then ((freeTVars t1) ++ (freeTVars t2)) 
                  else (freeTVars t2)
    TVar tv    -> [tv]
    TList t1   -> freeTVars t1

-- | Free type variables of a poly-type (remove forall-bound vars)
instance HasTVars Poly where
  freeTVars s     = case s of
    Mono t1         -> freeTVars t1
    Forall tv ply   -> freeTVars ply L.\\ [tv]

-- | Free type variables of a type environment
instance HasTVars TypeEnv where
  freeTVars gamma   = concat [freeTVars s | (x, s) <- gamma]  
  
-- | Lookup a variable in the type environment  
lookupVarType :: Id -> TypeEnv -> Poly
lookupVarType x ((y, s) : gamma)
  | x == y    = s
  | otherwise = lookupVarType x gamma
lookupVarType x [] = throw (Error ("unbound variable: " ++ x))

-- | Extend the type environment with a new biding
extendTypeEnv :: Id -> Poly -> TypeEnv -> TypeEnv
extendTypeEnv x s gamma = (x,s) : gamma  

-- | Lookup a type variable in a substitution;
--   if not present, return the variable unchanged
lookupTVar :: TVar -> Subst -> Type
lookupTVar a []                   = (TVar a)
lookupTVar a ((b, t) : next) = if (a == b) then t
                                else (lookupTVar a next)

-- | Remove a type variable from a substitution
removeTVar :: TVar -> Subst -> Subst
removeTVar a sub = sub L.\\ ((a, (lookupTVar a sub)) : [])
     
-- | Things to which type substitutions can be apply
class Substitutable a where
  apply :: Subst -> a -> a
  
-- | Apply substitution to type
instance Substitutable Type where  
  apply sub t         = case t of
    TInt              -> t
    TBool             -> t 
    t1 :=> t2         -> ((apply sub t1) :=> (apply sub t2))
    TVar tv           -> (lookupTVar tv sub)
    TList t1          -> (TList (apply sub t1))

-- | Apply substitution to poly-type
instance Substitutable Poly where    
  apply sub s         = case s of
    Mono t1           -> (Mono (apply sub t1))
    Forall tv ply     -> (Forall tv (apply sub ply))

-- | Apply substitution to (all poly-types in) another substitution
instance Substitutable Subst where  
  apply sub to = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip to
      
-- | Apply substitution to a type environment
instance Substitutable TypeEnv where  
  apply sub gamma = zip keys $ map (apply sub) vals
    where
      (keys, vals) = unzip gamma
      
-- | Extend substitution with a new type assignment
extendSubst :: Subst -> TVar -> Type -> Subst
extendSubst sub a t = 
  let
      ext s sub'      = case sub' of
        []                -> []
        ((tv, t) : next)  -> ((tv, apply s t) : []) ++ (ext s next)
        _                 -> throw (Error ("Error: type error extendSubst"))
    in (a, apply sub t) : (ext ((a, apply sub t) : []) sub)
      
--------------------------------------------------------------------------------
-- Problem 2: Unification
--------------------------------------------------------------------------------
      
-- | State of the type inference algorithm      
data InferState = InferState { 
    stSub :: Subst -- ^ current substitution
  , stCnt :: Int   -- ^ number of fresh type variables generated so far
} deriving (Eq,Show)

-- | Initial state: empty substitution; 0 type variables
initInferState = InferState [] 0

-- | Fresh type variable number n
freshTV n = TVar $ "a" ++ show n      
    
-- | Extend the current substitution of a state with a new type assignment   
extendState :: InferState -> TVar -> Type -> InferState
extendState (InferState sub n) a t = InferState (extendSubst sub a t) n
        
-- | Unify a type variable with a type; 
--   if successful return an updated state, otherwise throw an error
unifyTVar :: InferState -> TVar -> Type -> InferState
unifyTVar st a t = case t of
  TVar a          -> st
  _               -> if(L.elem a (freeTVars t) /= True) then (extendState st a t)
                    else throw (Error ("type error: cannot unify " ++ a ++ " and " ++ (show t) ++ " (occurs check)"))
    
-- | Unify two types;
--   if successful return an updated state, otherwise throw an error
unify :: InferState -> Type -> Type -> InferState
unify st (TInt) (TBool)       = throw (Error ("type error: cannot unify Int and Bool"))

unify st t1 t2 = case t1 of
  (TInt)      -> case t2 of
      (TInt)      -> st
      (TVar id)   -> unifyTVar st id t1
      _               -> throw (Error ("type error in unify"))
  (TBool)     -> case t2 of
      (TBool)         -> st
      (TVar id)       -> unifyTVar st id t1
      _               -> throw (Error ("type error in unify"))
  (t1a :=> t1b) -> case t2 of
      (t1a' :=> t1b') -> st'
        where
          InferState sub cnt  = unify st t1a t1a'
          (tx, tx2)           = (apply sub t1b, apply sub t1b')
          st'                 = unify (InferState sub cnt) tx tx2
      (TVar id)       -> unifyTVar st id t1
      _               -> throw (Error ("type error in unify"))

  (TVar id)   -> unifyTVar st id t2

  (TList ty)  -> case t2 of
      (TList ty')     -> unify st ty ty'
      (TVar id)       -> unifyTVar st id t1
      _               -> throw (Error ("type error in unify"))

  _             -> throw (Error ("type error in unify"))


--------------------------------------------------------------------------------
-- Problem 3: Type Inference
--------------------------------------------------------------------------------    
  
infer :: InferState -> TypeEnv -> Expr -> (InferState, Type)
infer st _   (EInt _)          = (st, TInt)
infer st _   (EBool _)         = (st, TBool)
infer st gamma (EVar x)        = 
  let
    fCount          = fst (instantiate (stCnt st) ply)
      where
        ply               = lookupVarType x gamma
    t1              = snd (instantiate (stCnt st) ply)
      where
        ply               = lookupVarType x gamma
    st'             = InferState (stSub st) fCount
  in (st', t1)
infer st gamma (ELam x body)   = 
  let
    st'             = fst(infer st'' gamma' body)
      where
        st''              = InferState (stSub st) ((stCnt st) + 1)
        gamma'            = extendTypeEnv x (Mono t1') gamma
          where 
            t1'              = freshTV (stCnt st)
    t1              = apply (stSub st') t1'
      where
        t1'               = freshTV (stCnt st)
    body'           = snd(infer st'' gamma' body)
      where
        st''              = InferState (stSub st) ((stCnt st) + 1)
        gamma'            = extendTypeEnv x (Mono t1') gamma
          where
            t1'              = freshTV (stCnt st)
  in (st', t1 :=> body')
infer st gamma (EApp e1 e2)    = 
  let
    st2             = fst (infer st1 gamma' e2)
      where
        st1               = fst (infer st gamma e1)
        gamma'            = apply (stSub st1) gamma
    st0             = unify st3 te1 (te2 :=> t1')
      where
        st3               = InferState (stSub st2) (stCnt st2 + 1)
        te1               = snd (infer st gamma e1)
        te2               = snd (infer st1 gamma' e2)
          where
            st1             = fst (infer st gamma e1)
            gamma'          = apply (stSub st1) gamma
        t1'               = (freshTV (stCnt st2))
    t1              = apply (stSub st0) t1'
      where
        t1'               = (freshTV (stCnt st2))
  in (st0, t1)
infer st gamma (ELet x e1 e2)  = 
  let
    t1b             = generalize gamma' t1a
      where
        gamma'          = apply (stSub st1) gamma
          where
            st1             = fst (infer st gamma e1)
        t1a             = snd (infer st gamma e1)
    st0             = fst (infer st1 gamma'' e2)
      where
        st1             = fst (infer st gamma e1)
        gamma''         = extendTypeEnv x t1b gamma'
          where
            st1             = fst (infer st gamma e1)
            gamma'          = apply (stSub st1) gamma
    t1             = snd (infer st1 gamma'' e2)
      where
        st1             = fst (infer st gamma e1)
        gamma''         = extendTypeEnv x t1b gamma'
          where
            st1             = fst (infer st gamma e1)
            gamma'          = apply (stSub st1) gamma
  in (st0, t1)
infer st gamma (EBin op e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp opVar e1) e2
    opVar = EVar (show op)
infer st gamma (EIf c e1 e2) = infer st gamma asApp
  where
    asApp = EApp (EApp (EApp ifVar c) e1) e2
    ifVar = EVar "if"    
infer st gamma ENil = infer st gamma (EVar "[]")

-- | Generalize type variables inside a type
generalize :: TypeEnv -> Type -> Poly
generalize gamma t = 
  let t1 = freeTVars t L.\\ (freeTVars gamma)
  in generalizeH t1
    where
      generalizeH t1 = case t1 of
        []            -> Mono t
        (x:xs)        -> Forall x (generalizeH xs)
    
-- | Instantiate a polymorphic type into a mono-type with fresh type variables
instantiate :: Int -> Poly -> (Int, Type)
instantiate n s = 
  let t1 = (InferState [] n)
  in instH t1 s
    where
      instH t1' s' = case s' of
        Mono m        -> (stc, apply (stSub t1') m)
          where
            stc       = stCnt t1'
        Forall tv t   -> instH (InferState sts (stc + 1)) t
          where
            ftv = freshTV (stCnt t1')
            extState  = extendState t1' tv (ftv)
            sts       = stSub extState
            stc       = stCnt t1'
      
-- | Types of built-in operators and functions      
preludeTypes :: TypeEnv
preludeTypes =
  [ ("+",    Mono $ TInt          :=> TInt          :=> TInt)
  , ("-",    Mono $ TInt          :=> TInt          :=> TInt)
  , ("*",    Mono $ TInt          :=> TInt          :=> TInt)
  , ("/",    Mono $ TInt          :=> TInt          :=> TInt)
  , ("==",   Mono $ (TVar "tv0")  :=> (TVar "tv0")  :=> TBool)
  , ("!=",   Mono $ (TVar "tv1")  :=> (TVar "tv1")  :=> TBool)
  , ("<",    Mono $ TInt          :=> TInt          :=> TBool)
  , ("<=",   Mono $ TInt          :=> TInt          :=> TBool)
  , ("&&",   Mono $ TBool         :=> TBool         :=> TBool)
  , ("||",   Mono $ TBool         :=> TBool         :=> TBool)
  , ("if",   Mono $ TBool)
  -- lists: 
  , ("[]",   Mono $ (TVar "tv2")        :=> TList (TVar "tv2"))
  , (":",    Mono $ (TVar "tv3")        :=> TList (TVar "tv3")    :=> TList (TVar "tv3"))
  , ("head", Mono $ TList (TVar "tv4")  :=> (TVar "tv4"))
  , ("tail", Mono $ TList (TVar "tv5")  :=> TList (TVar "tv5"))
  ]
