module Interpreter.Small where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.TypeTesting
import Interpreter.Operations
import Interpreter.Types
import Parser.Types
import System.Exit
import Text.Printf

evalPgm :: Pgm -> Ans
evalPgm (Program c) = evalCom c env (\_ -> return ExitSuccess) store
  where
    env = HashMap.empty
    store = Store HashMap.empty 0

evalRVal :: Exp -> Env -> Ec -> Cc
evalRVal e1 r k = evalExp e1 r $ deref $ testRv e1 k

evalExp :: Exp -> Env -> Ec -> Cc
evalExp (Int x) r k s = k (DInt x) s
evalExp (Double x) r k s = k (DDouble x) s
evalExp (Bool x) r k s = k (DBool x) s
evalExp (String x) r k s = k (DString x) s
evalExp Read r k s = do
  input <- getLine
  k (DString input) s
evalExp (I i1) r k s = isUnboundEnv i1 r ?> (putError $ printf "\"%s\" is unassigned\"" i1, k (lookupEnv i1 r) s)
evalExp (Func e1 e2) r k s = evalExp e1 r (testFunc e1 (\f -> evalExp e2 r $ evToFunc f k)) s
evalExp (IfExp e1 e2 e3) r k s = evalRVal e1 r (testBool e1 $ \e -> cond (evalExp e2 r k, evalExp e3 r k) $ evToBool e) s
evalExp (Op o1 e1 e2) r k s = evalRVal e1 r (\e1 -> evalRVal e2 r (\e2 -> evalOp ef o1 (evToRv e1, evToRv e2) k)) s
  where
    ef = Op o1 e1 e2

evalCom :: Com -> Env -> Cc -> Cc
evalCom (Assign e1 e2) r c = evalExp e1 r $ testLoc e1 (\l -> evalRVal e2 r $ update (evToLoc l) c)
evalCom (Output e1) r c = evalRVal e1 r (\e s -> putStrLn (pretty e) >> c s)
evalCom (Proc e1 e2) r c = evalExp e1 r $ testProc e1 (\p -> evalExp e2 r $ evToProc p c)
evalCom (If e1 c1 c2) r c = evalRVal e1 r $ testBool e1 $ \e -> cond (evalCom c1 r c, evalCom c2 r c) $evToBool e
evalCom (While e1 c1) r c = evalRVal e1 r $ testBool e1 $ \e -> cond (evalCom c1 r $ evalCom (While e1 c1) r c, c) $evToBool e
evalCom (Block d1 c1) r c = evalDec d1 r (\r' -> evalCom c1 (updateEnv r' r) c)
evalCom (Trap cs is) r c = evalCom (head cs) (updateEnv (newEnvMulti is ccs) r) c
  where
    ccs = map (\c' -> DCc (evalCom c' r c)) $ tail cs
evalCom (Escape i1) r c = evalExp (I i1) r $ testCc (I i1) (\(DCc c) -> c)
evalCom (Chain c1 c2) r c = evalCom c1 r $ evalCom c2 r c
evalCom Skip r c = c

evalDec :: Dec -> Env -> Dc -> Cc
evalDec (Const i1 e1) r u = evalRVal e1 r (u . newEnv i1)
evalDec (Var i1 e1) r u = evalRVal e1 r $ ref (u . newEnv i1)
evalDec (ProcDec i1 i2 c1) r u = u (newEnv i1 procd)
  where
    procd' c e = evalCom c1 (updateEnv (newEnv i2 e) r) c
    procd = DProc procd'
evalDec (FuncDec i1 i2 e1) r u = u (newEnv i1 func)
  where
    func' k e = evalExp e1 (updateEnv (newEnv i2 e) r) k
    func = DFunc func'
evalDec (ChainDec d1 d2) r u = evalDec d1 r (\r1 -> evalDec d2 (updateEnv r r1) (\r2 -> u (updateEnv r2 r1)))
evalDec SkipDec r u = u HashMap.empty

interpretSmall :: Pgm -> Ans
interpretSmall = evalPgm