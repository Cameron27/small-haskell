module Interpreter.Small (interpretSmall) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.BasicOperations
import Interpreter.DefaultEnvironment
import Interpreter.Helper.Array
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Interpreter.Types
import Parser.Types
import System.Exit
import Text.Printf

evalPgm :: Pgm -> Ans
evalPgm (Program c) = do
  evalOwnCom c w emptyEnv (\r -> evalCom c w (updateEnv r env) (\_ -> return ExitSuccess)) store
  where
    env = defaultEnv
    store = Store HashMap.empty 0
    w = Posn 1

evalRVal :: Exp -> Posn -> Env -> Ec -> Cc
evalRVal e1 w r k = evalExp e1 w r $ deref $ testRv e1 k

evalExp :: Exp -> Posn -> Env -> Ec -> Cc
evalExp (Int x) w r k s = k (DInt x) s
evalExp (Double x) w r k s = k (DDouble x) s
evalExp (Bool x) w r k s = k (DBool x) s
evalExp (String x) w r k s = k (DString x) s
evalExp Read w r k s = do
  input <- getLine
  k (DString input) s
evalExp (I i1) w r k s = isUnboundEnv i1 r ?> (putError $ printf "\"%s\" is unassigned\"" i1, k (lookupEnv i1 r) s)
evalExp (RefExp e1) w r k s = (evalExp e1 (w ! 1) r $ ref k) s
evalExp (ArrayExp e1 e2 _) w r k s =
  ( evalRVal e1 (w ! 1) r $
      testInt
        e1
        ( \n1 ->
            evalRVal e2 (w ! 2) r $
              testInt
                e2
                (\n2 -> newArray (dvToInt n1, dvToInt n2) k)
        )
  )
    s
evalExp (RecordExp is _) w r k s = k (DRecord record) s'
  where
    (ls', s') = newLocsStore (length is) s
    ls = map DLoc ls'
    (Env record' _ _) = newEnvMulti is ls
    record = Record record'
evalExp (Func e1 e2) w r k s = evalExp e1 (w ! 1) r (testFunc (length e2) (Func e1 e2) chainEval) s
  where
    params = zip e2 [2 ..]
    chainEval :: Ev -> Cc
    chainEval p =
      foldr
        (\(e, i) x es -> evalExp e (w ! i) r (\e -> x (es ++ [e])))
        (evToFunc p k)
        params
        []
evalExp (IfExp e1 e2 e3) w r k s = evalRVal e1 (w ! 1) r (testBool e1 $ \e -> cond (evalExp e2 (w ! 2) r k, evalExp e3 (w ! 3) r k) $ evToBool e) s
evalExp (Valof t1 c1) w r k s = evalCom c1 (w ! 1) (Env r' w' k) (err $ printf "no return encountered in %s" $ show (Valof t1 c1)) s
  where
    (Env r' w' _) = r
evalExp (Cont e1) w r k s = (evalExp e1 (w ! 1) r $ testLoc e1 $ cont k) s
evalExp (ArrayAccess e1 e2) w r k s =
  ( evalRVal e1 (w ! 1) r $
      testArray
        e1
        (\e1 -> evalRVal e2 (w ! 2) r $ testInt e2 $ arrayAccess (dvToArray e1) k)
  )
    s
evalExp (Dot e1 e2) w r k s = (evalRVal e1 (w ! 1) r $ testRecord e1 (\r' -> evalExp e2 (w ! 2) (updateEnv (recordToEnv $ dvToRecord r') r) k)) s
evalExp (Not e1) w r k s = (evalRVal e1 (w ! 1) r $ testBool e1 (\e -> k (DBool (not $ dvToBool e)))) s
evalExp (Op o1 e1 e2) w r k s = evalRVal e1 (w ! 2) r (\e1 -> evalRVal e2 (w ! 3) r (\e2 -> evalOp ef o1 (evToRv e1, evToRv e2) k)) s
  where
    ef = Op o1 e1 e2

evalCom :: Com -> Posn -> Env -> Cc -> Cc
evalCom (Assign e1 e2) w r c = evalExp e1 (w ! 1) r $ testLoc e1 (\l -> evalRVal e2 (w ! 2) r $ update (evToLoc l) c)
evalCom (Output e1) w r c = evalRVal e1 (w ! 1) r (\e s -> putStrLn (print e) >> c s)
  where
    print e = case e of
      DString s -> s
      e -> show e
evalCom (Proc e1 e2) w r c = evalExp e1 (w ! 1) r $ testProc (length e2) (Proc e1 e2) chainEval
  where
    params = zip e2 [2 ..]
    chainEval p =
      foldr
        (\(e, i) x es -> evalExp e (w ! i) r (\e -> x (es ++ [e])))
        (evToProc p c)
        params
        []
evalCom (If e1 c1 c2) w r c = evalRVal e1 (w ! 1) r $ testBool e1 $ \e -> cond (evalCom c1 (w ! 2) r c, evalCom c2 (w ! 3) r c) $evToBool e
evalCom (While e1 c1) w r c = evalRVal e1 (w ! 1) r $ testBool e1 $ \e -> cond (evalCom c1 (w ! 2) r $ evalCom (While e1 c1) w r c, c) $evToBool e
evalCom (Repeat e1 c1) w r c = evalCom c1 (w ! 2) r $ evalRVal e1 (w ! 1) r $ testBool e1 (\e -> dvToBool e ?> (c, evalCom (Repeat e1 c1) w r c))
evalCom (For i1 f1 c1) w r c = evalExp (I i1) (w ! 1) r $ testLoc (I i1) (\l -> evalFor f1 (w ! 2) r (\c' [e] -> update (dvToLoc l) (evalCom c1 (w ! 3) r c') e) c)
evalCom (Block d1 c1) w r c = evalDec d1 (w ! 1) r (\r' -> evalCom c1 (w ! 2) (updateEnv r' r) c)
evalCom (Trap cs is) w r c = evalCom (head cs) (w ! 1) (updateEnv (newEnvMulti is ccs) r) c
  where
    ccs =
      zipWith
        (\c' i -> DCc (evalCom c' (w ! i) r c))
        (tail cs)
        [2 ..]
evalCom (Escape i1) w r c = evalExp (I i1) (w ! 1) r $ testCc (I i1) (\(DCc c) -> c)
evalCom (Return e1) w r c = evalRVal e1 (w ! 1) r k
  where
    (Env _ _ k) = r
evalCom (WithDo e1 c1) w r c = evalRVal e1 (w ! 1) r $ testRecord e1 (\r' -> evalCom c1 (w ! 2) (updateEnv (recordToEnv $ dvToRecord r') r) c)
evalCom (Chain c1 c2) w r c = evalCom c1 (w ! 1) r $ evalCom c2 (w ! 2) r c
evalCom Skip w r c = c

evalDec :: Dec -> Posn -> Env -> Dc -> Cc
evalDec (Const i1 _ e1) w r u s = evalRVal e1 (w ! 2) r (u . newEnv i1) s
evalDec (Var i1 _ e1) w r u s = (evalRVal e1 (w ! 2) r $ ref (u . newEnv i1)) s
evalDec (Own i1 _ e1) w r u s = u (newEnv i1 (lookupEnvOwn (i1, w) r)) s
evalDec (ArrayDec i1 e1 e2 _) w r u s =
  ( evalRVal e1 (w ! 2) r $
      testInt
        e1
        ( \n1 ->
            evalRVal e2 (w ! 3) r $
              testInt
                e2
                (\n2 -> newArray (evToInt n1, evToInt n2) $ u . newEnv i1)
        )
  )
    s
evalDec (RecordDec i1 is _) w r u s = u (newEnv i1 (DRecord record)) s'
  where
    (ls', s') = newLocsStore (length is) s
    ls = map DLoc ls'
    (Env record' _ _) = newEnvMulti is ls
    record = Record record'
evalDec (ProcDec i1 i2 _ c1) w r u s = u (newEnv i1 procd) s
  where
    procd' c e = evalCom c1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) c
    procd = DProc procd' (length i2)
evalDec (RecProcDec i1 i2 _ c1) w r u s = u (newEnv i1 procd) s
  where
    procd = DProc (\c e -> evalCom c1 (w ! 3) (updateEnv (newEnvMulti (i1 : i2) (procd : e)) r) c) (length i2)
evalDec (FuncDec i1 i2 _ _ e1) w r u s = u (newEnv i1 func) s
  where
    func' k e = evalExp e1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) k
    func = DFunc func' (length i2)
evalDec (RecFuncDec i1 i2 _ _ e1) w r u s = u (newEnv i1 func) s
  where
    func = DFunc (\k e -> evalExp e1 (w ! 3) (updateEnv (newEnvMulti (i1 : i2) (func : e)) r) k) (length i2)
evalDec (FileDec i1 i2 _) w r u s = u (newEnvMulti [i1, i2] ls) (updateStore l1 (Just $ SFile $ File [] 1 l2) s')
  where
    (ls', s') = newLocsStore 2 s
    ls = map DLoc ls'
    [l1, l2] = ls'
evalDec (ChainDec d1 d2) w r u s = evalDec d1 (w ! 1) r (\r1 -> evalDec d2 (w ! 2) (updateEnv r1 r) (\r2 -> u (updateEnv r2 r1))) s
evalDec SkipDec w r u s = u (Env HashMap.empty HashMap.empty emptyEc) s

evalFor :: For -> Posn -> Env -> Procedure -> Cc -> Cc
evalFor (ExpFor e1) w r p c = evalRVal e1 (w ! 1) r $ \e -> p c [e]
evalFor (WhileFor e1 e2) w r p c =
  evalRVal
    e1
    (w ! 1)
    r
    ( \e ->
        evalRVal e2 (w ! 2) r $
          testBool
            e2
            (\b -> dvToBool b ?> (p (evalFor (WhileFor e1 e2) w r p c) [e], c))
    )
evalFor (StepFor e1 e2 e3) w r p c = evalRVal e1 (w ! 1) r $ testInt e1 $ step (evalRVal e2 (w ! 2) r, evalRVal e3 (w ! 3) r) p c . dvToInt
  where
    step :: (Ec -> Cc, Ec -> Cc) -> Procedure -> Cc -> Int -> Cc
    step (w1, w2) p c n =
      w1 $
        testInt
          e2
          ( \n1 ->
              w2 $
                testInt
                  e3
                  ( \n2 ->
                      ((n - dvToInt n2) * signum (dvToInt n1) < 1)
                        ?> ( p (step (w1, w2) p c (n + dvToInt n1)) [DInt n],
                             c
                           )
                  )
          )
evalFor (ChainFor f1 f2) w r p c = evalFor f1 (w ! 1) r p $ evalFor f2 (w ! 2) r p c

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

interpretSmall :: Pgm -> Ans
interpretSmall = evalPgm