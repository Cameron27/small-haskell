module Interpreter.Core.Com where

import {-# SOURCE #-} Interpreter.Core.Dec
import {-# SOURCE #-} Interpreter.Core.Exp
import Interpreter.Core.Types
import Interpreter.Features.For
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.TypeTesting
import Parser.Core.Types

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
evalCom (For i1 f1 c1) w r c = evalForCom (For i1 f1 c1) w r c
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