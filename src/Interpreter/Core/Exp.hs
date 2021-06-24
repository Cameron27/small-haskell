module Interpreter.Core.Exp where

import {-# SOURCE #-} Interpreter.Core.Com
import Interpreter.Core.Types
import Interpreter.Features.BasicOperations
import Interpreter.Features.Classes
import Interpreter.Helper.Array
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Parser.Core.Types
import System.IO
import Text.Printf

-- | @evalExp exp w r k s@ evaluates the right hand value of expression `exp` under the environment `r` and with store
-- `s` then passes the resulting value into the rest of the program `k`.
evalRVal :: Exp -> Posn -> Env -> Ec -> Cc
evalRVal e1 w r k = evalExp e1 w r $ deref $ testRv e1 k

-- | @evalExp exp w r k s@ evaluates the expression `exp` under the environment `r` and with store `s` then passes the
-- resulting value into the rest of the program `k`.
evalExp :: Exp -> Posn -> Env -> Ec -> Cc
evalExp (Int x) w r k s = k (EInt x) s
evalExp (Double x) w r k s = k (EDouble x) s
evalExp (Bool x) w r k s = k (EBool x) s
evalExp (String x) w r k s = k (EString x) s
evalExp Read w r k s = do
  eof <- isEOF
  if eof
    then putError "no remaining input."
    else do
      input <- getLine
      k (EString input) s
evalExp (I i1) w r k s = case lookupEnv i1 r of
  Dv e -> k e s
  Unbound -> putError $ printf "\"%s\" is unbound." i1
evalExp (RefExp e1) w r k s = (evalExp e1 (w ! 1) r $ ref k) s
evalExp (ArrayExp e1 e2 _) w r k s =
  ( evalRVal e1 (w ! 1) r $
      testInt
        e1
        ( \n1 ->
            evalRVal e2 (w ! 2) r $
              testInt
                e2
                (\n2 -> newArray (evToInt n1, evToInt n2) k)
        )
  )
    s
evalExp (RecordExp is _) w r k s = k (ERecord record) s'
  where
    (ls', s') = newLocsStore (length is) s
    ls = map ELoc ls'
    (Env record' _ _ _) = newEnvMulti is ls
    record = Record record'
evalExp (Func e1 es) w r k s = evalExp e1 (w ! 1) r (testFunc (length es) (Func e1 es) chainEval) s
  where
    params = zip es [2 ..]
    chainEval p =
      foldr
        (\(e, i) f' es' -> evalExp e (w ! i) r $ testDv e (\e' -> f' (es' ++ [e']))) -- Evaluate the expression, add it to list and pass it on
        (evToFunc p k) -- End by running the function
        params -- Apply to each expression
        [] -- Start with an empty list of values
evalExp (IfExp e1 e2 e3) w r k s = evalRVal e1 (w ! 1) r (testBool e1 $ \e -> cond (evalExp e2 (w ! 2) r k, evalExp e3 (w ! 3) r k) $ evToBool e) s
evalExp (Valof t1 c1) w r k s = evalCom c1 (w ! 1) (Env r' w' k t') (err $ printf "no return encountered in \"%s\"." $ show (Valof t1 c1)) s
  where
    (Env r' w' _ t') = r
evalExp (Cont e1) w r k s = (evalExp e1 (w ! 1) r $ testLoc e1 $ cont k) s
evalExp (ArrayAccess e1 e2) w r k s =
  ( evalRVal e1 (w ! 1) r $
      testArray
        e1
        (\e1 -> evalRVal e2 (w ! 2) r $ testInt e2 $ arrayAccess (evToArray e1) k)
  )
    s
evalExp (Dot e1 e2) w r k s = evalDotExp (Dot e1 e2) w r k s
evalExp (New i1) w r k s = evalNewExp (New i1) w r k s
evalExp This w r k s = evalThisExp This w r k s
evalExp Null w r k s = evalNullExp Null w r k s
evalExp (Op2 o1 e1 e2) w r k s = evalRVal e1 (w ! 2) r (\e1 -> evalRVal e2 (w ! 3) r (\e2 -> evalOp2 ef o1 (e1, e2) k)) s
  where
    ef = Op2 o1 e1 e2
evalExp (Op1 o1 e1) w r k s = evalRVal e1 (w ! 2) r (\e1 -> evalOp1 ef o1 e1 k) s
  where
    ef = Op1 o1 e1