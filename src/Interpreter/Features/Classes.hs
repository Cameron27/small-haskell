module Interpreter.Features.Classes where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Debug.Trace
import {-# SOURCE #-} Interpreter.Core.Com
import {-# SOURCE #-} Interpreter.Core.Dec
import {-# SOURCE #-} Interpreter.Core.Exp
import Interpreter.Core.Types
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.TypeTesting
import Parser.Core.Types
import Text.Printf

-- | @evalClassDec dec w r u s@ evaluates the class declaration `dec` under the environment `r` and with store `s` then
-- passes the resulting environment into the rest of the program `u`.
evalClassDec :: Dec -> Posn -> Env -> Dc -> Cc
evalClassDec (ClassDec i1 scd1) w r u (Store s' n) = u (newEnv i1 (c n)) (Store s' (n + 1))
  where
    c n = DClass $ Class $ \k -> evalSCDec scd1 (w ! 2) (updateEnv (newEnv i1 (c n)) r1) $ \(Env r1 _ _ _, Env r2 _ _ _) -> k (DObject $ Object r1 r2 n)
    (Env r1' w1' _ _) = r
    r1 = Env r1' w1' defaultReturn emptyObj

-- | @evalSCDec scdec w r v s@ evaluates the scoped class declaration `scdec` under the environment `r` and with store
-- `s` then passes the resulting environment into the rest of the program `v`.
evalSCDec :: SCDec -> Posn -> Env -> DDc -> Store -> Ans
evalSCDec (Public cd1) w r v = evalCDec cd1 (w ! 1) r $ \r' -> v (r', emptyEnv)
evalSCDec (Private cd1) w r v = evalCDec cd1 (w ! 1) r $ \r' -> v (emptyEnv, r')
evalSCDec (ChainSCDec scd1 scd2) w r v = evalSCDec scd1 (w ! 1) r $ \(r11, r12) -> evalSCDec scd2 (w ! 2) r $
  \(r21, r22) -> v (updateEnv r21 r11, updateEnv r22 r12)
evalSCDec SkipSCDec w r v = v (emptyEnv, emptyEnv)

-- | @evalCDec cdec w r u s@ evaluates the declaration `cdec` as a class declaration under the environment `r` and with
-- store `s` then passes the resulting environment into the rest of the program `u`.
evalCDec :: Dec -> Posn -> Env -> Dc -> Cc
evalCDec (ProcDec i1 i2 _ c1) w r u s = u (newEnv i1 meth) s
  where
    procd' r c e = evalCom c1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) c
    meth' k o = k $ DProc (procd' (updateThisEnv o r)) (length i2)
    meth = DMethod meth'
evalCDec (FuncDec i1 i2 _ _ e1) w r u s = u (newEnv i1 meth) s
  where
    func' r k e = evalExp e1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) k
    meth' k o = k $ DFunc (func' (updateThisEnv o r)) (length i2)
    meth = DMethod meth'
evalCDec d1 w r u s = evalDec d1 w r u s

-- | @evalNewExp exp w r k s@ evaluates the new expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalNewExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNewExp (New i1) w r k = evalExp (I i1) (w ! 1) r $ testClass (New i1) $ \(DClass (Class c)) -> c k

-- | @evalThisExp exp w r k s@ evaluates the this expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalThisExp :: Exp -> Posn -> Env -> Ec -> Cc
evalThisExp This w r k = k (DObject this)
  where
    (Env _ _ _ this) = r

-- | @evalNullExp exp w r k s@ evaluates the null expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalNullExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNullExp Null w r k = k DNull

-- | A function that returns true iff the value passed in was null.
isNullF :: Function
isNullF k [e] = deref (\e' -> isNull e' ?> (k (DBool True), k (DBool False))) e

-- | @evalDotExp exp w r k s@ evaluates the dot expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalDotExp :: Exp -> Posn -> Env -> Ec -> Cc
evalDotExp (Dot e1 e2) w r k = evalRVal e1 (w ! 1) r $ \e -> process e
  where
    process r'
      | isRecord r' = evalExp e2 (w ! 2) (updateEnv (recordToEnv $ dvToRecord r') r) k -- Record updates the environment
      | isObject r' =
        let (DObject (Object r1 r2 n)) = r'
         in let (Env _ _ _ (Object _ _ n')) = r
             in evalExp e2 (w ! 2) (updateEnv ((if n == n' then objectToPrivateEnv else objectToPublicEnv) $ dvToObject r') r) $ \e -> case e of -- Object updates the environment
                  (DMethod f) -> f k $ dvToObject r' -- Methods have "this" passed in
                  _ -> k e -- Anything else passed through
      | otherwise = err $ printf "\"%s\", evaluated as \"%s\", is not an object or a record." (pretty e1) (pretty r') -- Anything else is an error