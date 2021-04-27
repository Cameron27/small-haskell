module TypeChecker.Small (typeCheckSmall) where

import Common.Formatting
import Common.Functions
import Data.Foldable
import qualified Data.Set as Set
import Debug.Trace
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
  if t2 `assignable` t1
    then return ()
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t2) (show t1) (pretty (Assign e1 e2))
typeCom (Output e1) r = do
  t <- typeExp e1 r >>= rval (Output e1)
  if isPrintable t
    then return ()
    else err $ printf "\"%s\" is not a printable type in \"%s\"" (show t) (pretty (Output e1))
typeCom (Proc e1 es) r = do
  p <- typeExp e1 r
  ts <- foldr (\e ts -> do t <- typeExp e r; (t :) <$> ts) (Right []) es
  case p of
    TProc pts ->
      if (length ts == length pts) && and (zipWith eq ts pts)
        then return ()
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"" (show ts) (show pts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a procedure in \"%s\"" (show p) (pretty (Proc e1 es))
typeCom (If e1 c1 c2) r = do
  t <- typeExp e1 r >>= rval (If e1 c1 c2)
  if t `eq` TBool
    then do
      typeCom c1 r
      typeCom c2 r
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (If e1 c1 c2))
typeCom (While e1 c1) r = do
  t <- typeExp e1 r >>= rval (While e1 c1)
  if t `eq` TBool
    then typeCom c1 r
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (While e1 c1))
typeCom (Repeat e1 c1) r = do
  t <- typeExp e1 r >>= rval (Repeat e1 c1)
  if t `eq` TBool
    then typeCom c1 r
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (Repeat e1 c1))
typeCom (For i1 f1 c1) r = do
  t1 <- typeExp (I i1) r
  t2 <- typeFor f1 r
  if t2 `assignable` t1
    then typeCom c1 r
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t2) (show t1) (pretty (For i1 f1 c1))
typeCom (Trap cs is) r = do
  typeCom (head cs) (updateTEnv (newTEnvMulti is ts) r)
  foldl (\c1 c2 -> c1 >> typeCom c2 r) (Right ()) (tail cs)
  where
    ts = TEscape : ts
typeCom (Escape i1) r = do
  t <- lookupTEnv i1 r
  if t `eq` TEscape
    then return ()
    else err $ printf "escape cannot be applied to a \"%s\" in \"%s\"" (show t) (pretty (Escape i1))
typeCom (Return e1) r = do
  t <- typeExp e1 r >>= rval (Return e1)
  if t `eq` rt
    then return ()
    else err $ printf "return type \"%s\" does not match expected return type \"%s\" in \"%s\"" (show t) (show rt) (pretty (Return e1))
  where
    (TEnv _ rt) = r
typeCom (WithDo e1 c1) r = do
  t <- typeExp e1 r >>= rval (WithDo e1 c1)
  if t `eq` TRecordAny
    then typeCom c1 (updateTEnv (uncurry newTEnvMulti (unzip $ recordTypes t)) r)
    else err $ printf "with cannot be performed on type \"%s\" in \"%s\"" (show t) (pretty (WithDo e1 c1))
typeCom (Block d1 c1) r = do
  r1 <- typeDec d1 r
  typeCom c1 (updateTEnv r1 r)
typeCom (Chain c1 c2) r =
  do
    typeCom c1 r
    typeCom c2 r
typeCom Skip r = Right ()

typeDec :: Dec -> TEnv -> Either TypeError TEnv
typeDec (Const i1 t1 e1) r = do
  t <- typeExp e1 r >>= rval (Const i1 t1 e1)
  if t `eq` t1
    then return $ newTEnv i1 t
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1) (pretty (Const i1 t1 e1))
typeDec (Var i1 t1 e1) r = do
  t <- typeExp e1 r >>= rval (Var i1 t1 e1)
  t1' <- ref (Var i1 t1 e1) t1
  if t `assignable` t1'
    then return $ newTEnv i1 t1'
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1') (pretty (Var i1 t1 e1))
typeDec (Own i1 t1 e1) r = do
  t <- typeExp e1 defaultTEnv >>= rval (Own i1 t1 e1)
  t1' <- ref (Own i1 t1 e1) t1
  if t `assignable` t1'
    then return $ newTEnv i1 t1'
    else err $ printf "cannot assign \"%s\" to \"%s\" in \"%s\"" (show t) (show t1') (pretty (Own i1 t1 e1))
typeDec (ArrayDec i1 e1 e2 t1) r = do
  typeType t1
  te1 <- typeExp e1 r >>= rval (ArrayDec i1 e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayDec i1 e1 e2 t1)
  if te1 `eq` TInt && te2 `eq` TInt
    then newTEnv i1 <$> (TArray <$> ref (ArrayDec i1 e1 e2 t1) t1)
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayDec i1 e1 e2 t1))
typeDec (RecordDec i1 is ts) r = do
  typeTypes ts
  if allDifferent is
    then do
      ts' <- foldr (\t ts' -> do t' <- ref (RecordDec i1 is ts) t; (t' :) <$> ts') (Right []) ts
      return $ newTEnv i1 $ TRecord (zip is ts')
    else err $ printf "all identifiers must be unique in \"%s\"" (pretty (RecordDec i1 is ts))
typeDec (FileDec i1 i2 t1) r = do
  typeType t1
  t1' <- ref (FileDec i1 i2 t1) t1
  t2' <- ref (FileDec i1 i2 t1) (TFile t1)
  return $ newTEnvMulti [i1, i2] [t2', t1']
typeDec (ProcDec i1 is ts c1) r = do
  typeTypes ts
  typeCom c1 (updateTEnv (newTEnvMulti is ts) r)
  return $ newTEnv i1 (TProc ts)
typeDec (RecProcDec i1 is ts c1) r = do
  typeTypes ts
  typeCom c1 (updateTEnv (newTEnvMulti (i1 : is) (TProc ts : ts)) r)
  return $ newTEnv i1 (TProc ts)
typeDec (FuncDec i1 is ts t1 e1) r = do
  typeTypes ts
  typeType t1
  t <- typeExp e1 (updateTEnv (newTEnvMulti is ts) r)
  if t `eq` t1
    then return $ newTEnv i1 (TFunc ts t1)
    else err $ printf "function result \"%s\" does not match type \"%s\" in \"%s\"" (show t) (show t1) (pretty (FuncDec i1 is ts t1 e1))
typeDec (RecFuncDec i1 is ts t1 e1) r = do
  typeTypes ts
  typeType t1
  t <- typeExp e1 (updateTEnv (newTEnvMulti (i1 : is) (TFunc ts t1 : ts)) r)
  if t `eq` t1
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
typeExp (RefExp e1) r = typeExp e1 r >>= ref (RefExp e1)
typeExp (ArrayExp e1 e2 t1) r = do
  typeType t1
  te1 <- typeExp e1 r >>= rval (ArrayExp e1 e2 t1)
  te2 <- typeExp e2 r >>= rval (ArrayExp e1 e2 t1)
  if te1 `eq` TInt && te2 `eq` TInt
    then TArray <$> ref (ArrayExp e1 e2 t1) t1
    else err $ printf "array cannot have bounds of types \"%s:%s\" in \"%s\"" (show te1) (show te2) (pretty (ArrayExp e1 e2 t1))
typeExp (RecordExp is ts) r = do
  typeTypes ts
  if allDifferent is
    then do
      ts' <- foldr (\t ts' -> do t' <- ref (RecordExp is ts) t; (t' :) <$> ts') (Right []) ts
      return $ TRecord (zip is ts')
    else err $ printf "all identifiers must be unique in \"%s\"" (pretty (RecordExp is ts))
typeExp (Func e1 es) r = do
  f <- typeExp e1 r
  ts <- foldr (\e ts -> do t <- typeExp e r; (t :) <$> ts) (Right []) es
  case f of
    TFunc fts t ->
      if (length ts == length fts) && and (zipWith eq ts fts)
        then return t
        else err $ printf "types \"%s\" did not match expected types \"%s\" in \"%s\"" (show ts) (show fts) (pretty (Proc e1 es))
    _ -> err $ printf "\"%s\" is not a function in \"%s\"" (show f) (pretty (Proc e1 es))
typeExp (IfExp e1 e2 e3) r = do
  t <- typeExp e1 r >>= rval (IfExp e1 e2 e3)
  if t `eq` TBool
    then do
      t1 <- typeExp e2 r
      t2 <- typeExp e3 r
      tryMerge (IfExp e1 e2 e3) t1 t2
    else err $ printf "test cannot be \"%s\" in \"%s\"" (show t) (pretty (IfExp e1 e2 e3))
typeExp (Valof t1 c1) (TEnv r _) = do
  typeType t1
  typeCom c1 (TEnv r t1)
  return t1
typeExp (Cont e1) r = do
  t <- typeExp e1 r
  if t `eq` TRefAny
    then return $ deref t
    else err $ printf "cont cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Cont e1))
typeExp (ArrayAccess e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (ArrayAccess e1 e2)
  t2 <- typeExp e2 r >>= rval (ArrayAccess e1 e2)
  if t1 `eq` TArrayAny
    then
      if t2 `eq` TInt
        then return $ arrayType t1
        else err $ printf "array access cannot have index of type \"%s\" in \"%s\"" (show t2) (pretty (ArrayAccess e1 e2))
    else err $ printf "array access cannot be performed on type \"%s\" in \"%s\"" (show t1) (pretty (ArrayAccess e1 e2))
typeExp (Dot e1 e2) r = do
  t <- typeExp e1 r >>= rval (Dot e1 e2)
  if t `eq` TRecordAny
    then typeExp e2 (updateTEnv (uncurry newTEnvMulti (unzip $ recordTypes t)) r)
    else err $ printf "dot operation be performed on type \"%s\" in \"%s\"" (show t) (pretty (Dot e1 e2))
typeExp (Not e1) r = do
  t <- typeExp e1 r >>= rval (Not e1)
  if t `eq` TBool
    then return TBool
    else err $ printf "! cannot be applied to type \"%s\" in \"%s\"" (show t) (pretty (Not e1))
typeExp (Op o1 e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (Op o1 e1 e2)
  t2 <- typeExp e2 r >>= rval (Op o1 e1 e2)
  typeOp (Op o1 e1 e2) o1 (t1, t2)

typeFor :: For -> TEnv -> Either TypeError Type
typeFor (ExpFor e1) r = do
  typeExp e1 r >>= rval (ExpFor e1)
typeFor (WhileFor e1 e2) r = do
  t1 <- typeExp e1 r >>= rval (WhileFor e1 e2)
  t2 <- typeExp e2 r >>= rval (WhileFor e1 e2)
  if t2 `eq` TBool
    then return t1
    else err $ printf "test cannot be type \"%s\" in \"%s\"" (show t2) (pretty (WhileFor e1 e2))
typeFor (StepFor e1 e2 e3) r = do
  t1 <- typeExp e1 r >>= rval (StepFor e1 e2 e3)
  t2 <- typeExp e2 r >>= rval (StepFor e1 e2 e3)
  t3 <- typeExp e3 r >>= rval (StepFor e1 e2 e3)
  if t1 `eq` TInt && t2 `eq` TInt && t3 `eq` TInt
    then return TInt
    else err $ printf "for step cannot have types \"%s\" in \"%s\"" (show [t1, t2, t3]) (pretty (StepFor e1 e2 e3))
typeFor (ChainFor f1 f2) r = do
  t1 <- typeFor f1 r
  t2 <- typeFor f2 r
  if t1 == t2
    then return t1
    else err $ printf "for cannot have mixed types \"%s\" and \"%s\" in \"%s\"" (show t1) (show t2) (pretty (ChainFor f1 f2))

typeType :: Type -> Either TypeError ()
typeType (TArray t) = typeType t
typeType (TRecord ts) =
  if allDifferent (map fst ts)
    then typeTypes (map snd ts)
    else err $ printf "all identifiers must be unique in \"%s\"" (show (TRecord ts))
typeType (TProc ts) = typeTypes ts
typeType (TFunc ts t) = do
  typeTypes ts
  typeType t
typeType (TFile t) = typeType t
typeType (TRef t) = typeType t
typeType (TRefMaybe t) = typeType t
typeType _ = return ()

typeTypes :: [Type] -> Either TypeError ()
typeTypes [] = return ()
typeTypes (t : ts) = do
  typeType t
  typeTypes ts

typeCheckSmall :: Pgm -> Maybe TypeError
typeCheckSmall p = case typePgm p of
  Right _ -> Nothing
  Left err -> Just err