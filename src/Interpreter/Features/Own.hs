module Interpreter.Features.Own (evalOwnCom) where

import Interpreter.Core.Exp
import Interpreter.Core.Types
import Interpreter.Helper.Continuation
import Interpreter.Helper.Env
import Parser.Core.Types

evalOwnCom :: Com -> Posn -> Env -> Dc -> Cc
evalOwnCom (Assign e1 e2) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (u . updateEnv r1))
evalOwnCom (Output e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnCom (Proc e1 es) w r u = evalOwnExp e1 (w ! 1) r chainEval
  where
    chainEval r =
      foldr
        (\(i, e1) x rs -> evalOwnExp e1 (w ! i) r (\r -> x (rs ++ [r])))
        (\rs -> u $ foldr updateEnv emptyEnv (r : rs))
        (zip [2 ..] es)
        []
evalOwnCom (If e1 c1 c2) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnCom c1 (w ! 2) r (\r2 -> evalOwnCom c2 (w ! 3) r (u . updateEnv (updateEnv r1 r2))))
evalOwnCom (While e1 c1) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnCom c1 (w ! 2) r (u . updateEnv r1))
evalOwnCom (Repeat e1 c1) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnCom c1 (w ! 2) r (u . updateEnv r1))
evalOwnCom (For _ f1 c1) w r u = evalOwnFor f1 (w ! 2) r (\r1 -> evalOwnCom c1 (w ! 3) r (u . updateEnv r1))
evalOwnCom (Block d1 c1) w r u = evalOwnDec d1 (w ! 1) r (\r1 -> evalOwnCom c1 (w ! 2) r (u . updateEnv r1))
evalOwnCom (Trap cs _) w r u =
  foldr
    (\(i, c1) x rs -> evalOwnCom c1 (w ! i) r (\r -> x (rs ++ [r])))
    (u . foldr updateEnv emptyEnv)
    (zip [1 ..] cs)
    []
evalOwnCom (Escape _) w r u = u emptyEnv
evalOwnCom (Return e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnCom (WithDo e1 c1) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnCom c1 (w ! 2) r (u . updateEnv r1))
evalOwnCom (Chain c1 c2) w r u = evalOwnCom c1 (w ! 1) r (\r1 -> evalOwnCom c2 (w ! 2) r (u . updateEnv r1))
evalOwnCom Skip w r u = u emptyEnv

evalOwnDec :: Dec -> Posn -> Env -> Dc -> Cc
evalOwnDec (Const _ _ e1) w r u = evalOwnExp e1 (w ! 2) r u
evalOwnDec (Var _ _ e1) w r u = evalOwnExp e1 (w ! 2) r u
evalOwnDec (Own i1 _ e1) w r u = evalOwnExp e1 (w ! 2) r (\r1 -> evalRVal e1 (w ! 2) (updateEnv r1 r) $ ref (u . updateEnv r1 . newEnvOwn (i1, w)))
evalOwnDec (ArrayDec _ e1 e2 _) w r u = evalOwnExp e1 (w ! 2) r (\r1 -> evalOwnExp e2 (w ! 3) r (u . updateEnv r1))
evalOwnDec (RecordDec _ _ _) w r u = u emptyEnv
evalOwnDec (FileDec _ _ _) w r u = u emptyEnv
evalOwnDec (ProcDec _ _ _ c1) w r u = evalOwnCom c1 (w ! 3) r u
evalOwnDec (RecProcDec _ _ _ c1) w r u = evalOwnCom c1 (w ! 3) r u
evalOwnDec (FuncDec _ _ _ _ e1) w r u = evalOwnExp e1 (w ! 3) r u
evalOwnDec (RecFuncDec _ _ _ _ e1) w r u = evalOwnExp e1 (w ! 3) r u
evalOwnDec (ChainDec d1 d2) w r u = evalOwnDec d1 (w ! 1) r (\r1 -> evalOwnDec d2 (w ! 2) r (u . updateEnv r1))
evalOwnDec SkipDec w r u = u emptyEnv

evalOwnExp :: Exp -> Posn -> Env -> Dc -> Cc
evalOwnExp (Int _) w r u = u emptyEnv
evalOwnExp (Double _) w r u = u emptyEnv
evalOwnExp (Bool _) w r u = u emptyEnv
evalOwnExp (String _) w r u = u emptyEnv
evalOwnExp Read w r u = u emptyEnv
evalOwnExp (I _) w r u = u emptyEnv
evalOwnExp (RefExp e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnExp (ArrayExp e1 e2 _) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (u . updateEnv r1))
evalOwnExp (RecordExp _ _) w r u = u emptyEnv
evalOwnExp (Func e1 es) w r u = evalOwnExp e1 (w ! 1) r chainEval
  where
    chainEval r =
      foldr
        (\(i, e1) x rs -> evalOwnExp e1 (w ! i) r (\r -> x (rs ++ [r])))
        (\rs -> u $ foldr updateEnv emptyEnv (r : rs))
        (zip [2 ..] es)
        []
evalOwnExp (IfExp e1 e2 e3) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (\r2 -> evalOwnExp e3 (w ! 3) r (u . updateEnv (updateEnv r1 r2))))
evalOwnExp (Valof _ c1) w r u = evalOwnCom c1 (w ! 1) r u
evalOwnExp (Cont e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnExp (ArrayAccess e1 e2) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (u . updateEnv r1))
evalOwnExp (Dot e1 e2) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (u . updateEnv r1))
evalOwnExp (Not e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnExp (Op _ e1 e2) w r u = evalOwnExp e1 (w ! 2) r (\r1 -> evalOwnExp e2 (w ! 3) r (u . updateEnv r1))

evalOwnFor :: For -> Posn -> Env -> Dc -> Cc
evalOwnFor (ExpFor e1) w r u = evalOwnExp e1 (w ! 1) r u
evalOwnFor (WhileFor e1 e2) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (u . updateEnv r1))
evalOwnFor (StepFor e1 e2 e3) w r u = evalOwnExp e1 (w ! 1) r (\r1 -> evalOwnExp e2 (w ! 2) r (\r2 -> evalOwnExp e3 (w ! 3) r (u . updateEnv (updateEnv r1 r2))))
evalOwnFor (ChainFor f1 f2) w r u = evalOwnFor f1 (w ! 1) r (\r1 -> evalOwnFor f2 (w ! 2) r (u . updateEnv r1))