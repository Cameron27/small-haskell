module TypeChecker.Small (typeCheckSmall) where

import Common.Formatting
import Data.Foldable
import qualified Data.Set as Set
import Parser.Types
import Text.Printf
import TypeChecker.BasicOperations
import TypeChecker.DefaultEnvironment
import TypeChecker.Helper.Control
import TypeChecker.Helper.TEnv
import TypeChecker.Helper.TypeModification
import TypeChecker.Types

typePgm :: Pgm -> Either TypeError ()
typePgm (Program c) = do typeCom c defaultTEnv; return ()

typeCom :: Com -> TEnv -> Either TypeError ()
typeCom (Assign e1 e2) r = do
  t1 <- typeExp e1 r
  t2 <- typeExp e2 r >>= rval (Assign e1 e2)
  if ref t2 `leq` t1
    then return ()
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t2) (show t1) (pretty (Assign e1 e2))
typeCom (Output e1) r = do
  t <- typeExp e1 r >>= rval (Output e1)
  if t `leq` TSet (Set.fromList [TInt, TDouble, TBool, TString])
    then return ()
    else err $ printf "\"%s\" is not a printable type in \"%s\"" (show t) (pretty (Output e1))
typeCom (Proc e1 es) r = do
  p <- typeExp e1 r
  let ts' = map (\e -> typeExp e r) es
  ts <- foldl (\ts t -> t >>= (\t -> (++ [t]) <$> ts)) (Right []) ts'
  case p of
    TProc pts ->
      if (length ts == length pts) && and (zipWith leq ts pts)
        then return ()
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"" (show ts) (show pts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a procedure in \"%s\"" (show p) (pretty (Proc e1 es))
typeCom (If e1 c1 c2) r = do
  t <- typeExp e1 r >>= rval (If e1 c1 c2)
  case t of
    TBool -> do
      typeCom c1 r
      typeCom c2 r
    _ -> err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (If e1 c1 c2))
typeCom (While e1 c1) r = do
  t <- typeExp e1 r >>= rval (While e1 c1)
  case t of
    TBool -> typeCom c1 r
    _ -> err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (While e1 c1))
typeCom (Repeat e1 c1) r = do
  t <- typeExp e1 r >>= rval (Repeat e1 c1)
  case t of
    TBool -> typeCom c1 r
    _ -> err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (Repeat e1 c1))
typeCom (For i1 f1 c1) r = do
  t1 <- typeExp (I i1) r
  t2 <- typeFor f1 r
  if ref t2 `leq` t1
    then typeCom c1 r
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t2) (show t1) (pretty (For i1 f1 c1))
typeCom (Block d1 c1) r = do
  r1 <- typeDec d1 r
  typeCom c1 (updateTEnv r1 r)
typeCom (Trap cs is) r = do
  typeCom (head cs) (updateTEnv (newTEnvMulti is ts) r)
  let res = map (\c -> typeCom c r) (tail cs)
  foldl (>>) (Right ()) res
  where
    ts = TEscape : ts
typeCom (Escape i1) r = do
  t <- lookupTEnv i1 r
  case t of
    TEscape -> return ()
    _ -> err $ printf "escape cannot be applied to a \"%s\" in \"%s\"" (show t) (pretty (Escape i1))
typeCom (Return e1) r = do
  t <- typeExp e1 r >>= rval (Return e1)
  if t `leq` rt
    then return ()
    else err $ printf "return type \"%s\" does not match expected return type \"%s\" in \"%s\"" (show t) (show rt) (pretty (Return e1))
  where
    (TEnv _ rt) = r
typeCom (WithDo e1 c1) r = do
  t <- typeExp e1 r >>= rval (WithDo e1 c1)
  case t of
    TRecord ts -> typeCom c1 (updateTEnv (uncurry newTEnvMulti (unzip ts)) r)
    _ -> err $ printf "with cannot be performed on type \"%s\" in \"%s\"" (show t) (pretty (WithDo e1 c1))
typeCom (Chain c1 c2) r =
  do
    typeCom c1 r
    typeCom c2 r
typeCom Skip r = Right ()

typeDec :: Dec -> TEnv -> Either TypeError TEnv
typeDec (Const i1 t1 e1) r = do
  t' <- typeExp e1 r
  t <- rval (Const i1 t1 e1) t'
  if t `leq` t1
    then return $ newTEnv i1 t
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1) (pretty (Const i1 t1 e1))
typeDec (Var i1 t1 e1) r = do
  t' <- typeExp e1 r >>= rval (Own i1 t1 e1)
  let t = ref t'
  if t `leq` ref t1
    then return $ newTEnv i1 $ ref t1
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t') (show (ref t1)) (pretty (Var i1 t1 e1))
typeDec (Own i1 t1 e1) r = do
  t' <- typeExp e1 defaultTEnv >>= rval (Own i1 t1 e1)
  let t = ref t'
  if t `leq` ref t1
    then return $ newTEnv i1 $ ref t1
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t') (show (ref t1)) (pretty (Own i1 t1 e1))
typeDec (ArrayDec i1 e1 e2 t1) r = do
  te1 <- typeExp e1 r >>= rval (ArrayDec i1 e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayDec i1 e1 e2 t1)
  if te1 == TInt && te2 == TInt
    then return $ newTEnv i1 (TArray $ ref t1)
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayDec i1 e1 e2 t1))
typeDec (RecordDec i1 is ts) r = return $ newTEnv i1 $ TRecord (zipWith (\i t -> (i, TRef t)) is ts)
typeDec (FileDec i1 i2 t1) r = Right $ newTEnvMulti [i1, i2] [ref (TFile t1), ref t1]
typeDec (ProcDec i1 is ts c1) r = do
  typeCom c1 (updateTEnv (newTEnvMulti is ts) r)
  return $ newTEnv i1 (TProc ts)
typeDec (RecProcDec i1 is ts c1) r = do
  typeCom c1 (updateTEnv (newTEnvMulti (i1 : is) (TProc ts : ts)) r)
  return $ newTEnv i1 (TProc ts)
typeDec (FuncDec i1 is ts t1 e1) r = do
  t <- typeExp e1 (updateTEnv (newTEnvMulti is ts) r)
  if t `leq` t1
    then return $ newTEnv i1 (TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeDec (RecFuncDec i1 is ts t1 e1) r = do
  t <- typeExp e1 (updateTEnv (newTEnvMulti (i1 : is) (TFunc ts t1 : ts)) r)
  if t `leq` t1
    then return $ newTEnv i1 (TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeDec (ChainDec d1 d2) r = do
  r1 <- typeDec d1 r
  r2 <- typeDec d2 (updateTEnv r1 r)
  return (updateTEnv r2 r1)
typeDec SkipDec r = Right emptyTEnv

typeExp :: Exp -> TEnv -> Either TypeError Type
typeExp (Int _) r = Right TInt
typeExp (Double _) r = Right TDouble
typeExp (Bool _) r = Right TBool
typeExp (String _) r = Right TString
typeExp Read r = Right TString
typeExp (I i1) r = lookupTEnv i1 r
typeExp (RefExp e1) r = ref <$> typeExp e1 r
typeExp (ArrayExp e1 e2 t1) r = do
  te1 <- typeExp e1 r >>= rval (ArrayExp e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayExp e1 e2 t1)
  if te1 `leq` TInt && te2 `leq` TInt
    then return $ TArray $ ref t1
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayExp e1 e2 t1))
typeExp (RecordExp is ts) r = return $ TRecord (zipWith (\i t -> (i, TRef t)) is ts)
typeExp (Func e1 es) r = do
  f <- typeExp e1 r
  let ts' = map (\e -> typeExp e r) es
  ts <- foldl (\ts t -> t >>= (\t -> (++ [t]) <$> ts)) (Right []) ts'
  case f of
    TFunc fts ft ->
      if and $ (length fts == length ts) : zipWith leq ts fts
        then return ft
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"" (show ts) (show fts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a function in \"%s\"" (show f) (pretty (Proc e1 es))
typeExp (IfExp e1 e2 e3) r = do
  t <- typeExp e1 r >>= rval (IfExp e1 e2 e3)
  if t `leq` TBool
    then do
      t1 <- typeExp e2 r
      t2 <- typeExp e3 r
      if t1 == t2
        then return t1
        else return $ createTSet [t1, t2]
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (IfExp e1 e2 e3))
typeExp (Jumpout i1 t1 e1) r = do
  t <- typeExp e1 (updateTEnv (newTEnv i1 (TFunc [t1] TJump)) r)
  if t1 == t
    then return t1
    else return $ createTSet [t, t1]
typeExp (Valof t1 c1) (TEnv r _) = do
  typeCom c1 (TEnv r t1)
  return t1
typeExp (Cont e1) r = do
  t <- typeExp e1 r
  case t of
    (TRef t) -> return t
    _ -> err $ printf "cont cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Cont e1))
typeExp (ArrayAccess e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (ArrayAccess e1 e2)
  t2 <- typeExp e2 r >>= rval (ArrayAccess e1 e2)
  if t1 `leq` TArray TAny
    then case t2 of
      TInt -> return $ dearray t1
      _ -> err $ printf "array access cannot have index of type \"%s\" in \"%s\"" (show t2) (pretty (ArrayAccess e1 e2))
    else err $ printf "array access cannot be performed on type \"%s\" in \"%s\"" (show t1) (pretty (ArrayAccess e1 e2))
typeExp (Dot e1 e2) r = do
  t <- typeExp e1 r >>= rval (Dot e1 e2)
  case t of
    TRecord ts -> typeExp e2 (updateTEnv (uncurry newTEnvMulti (unzip ts)) r)
    _ -> err $ printf "dot operation be performed on type \"%s\" in \"%s\"" (show t) (pretty (Dot e1 e2))
typeExp (Not e1) r = do
  t <- typeExp e1 r >>= rval (Not e1)
  case t of
    TBool -> return TBool
    _ -> err $ printf "! cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Not e1))
typeExp (Op o1 e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (Op o1 e1 e2)
  t2 <- typeExp e2 r >>= rval (Op o1 e1 e2)
  typeOp (Op o1 e1 e2) o1 (t1, t2)

typeFor :: For -> TEnv -> Either TypeError Type
typeFor (ExpFor e1) r = do
  t <- typeExp e1 r >>= rval (ExpFor e1)
  case t of
    TSet _ -> err $ printf "for cannot have the mixed type \"%s\" in \"%s\"" (show t) (pretty (ExpFor e1))
    _ -> return t
typeFor (WhileFor e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (WhileFor e1 e2)
  t2 <- typeExp e2 r >>= rval (WhileFor e1 e2)
  if t2 `leq` TBool
    then return t1
    else err $ printf "test cannot be type \"%s\" in \"%s\"" (show t2) (pretty (WhileFor e1 e2))
typeFor (StepFor e1 e2 e3) r = do
  t1 <- typeExp e1 r >>= rval (StepFor e1 e2 e3)
  t2 <- typeExp e2 r >>= rval (StepFor e1 e2 e3)
  t3 <- typeExp e3 r >>= rval (StepFor e1 e2 e3)
  if t1 `leq` TInt && t2 `leq` TInt && t3 `leq` TInt
    then return TInt
    else err $ printf "for step cannot have types \"%s\" in \"%s\"" (show [t1, t2, t3]) (pretty (StepFor e1 e2 e3))
typeFor (ChainFor f1 f2) r = do
  t1 <- typeFor f1 r
  t2 <- typeFor f2 r
  if t1 == t2
    then return t1
    else return $ createTSet [t1, t2]

typeCheckSmall :: Pgm -> Maybe TypeError
typeCheckSmall p = case typePgm p of
  Right _ -> Nothing
  Left err -> Just err