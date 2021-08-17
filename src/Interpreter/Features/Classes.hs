{-# LANGUAGE MultiWayIf #-}

module Interpreter.Features.Classes where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
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
evalClassDec (ClassDec i1 Nothing scd1) w r u = u (newEnv i1 c)
  where
    c = EClass . Class $ \k -> evalSCDec scd1 (w ! 3) (updateEnv (newEnv i1 c) r1) $ \o -> k (EObject o)
    (Env r1' w1' _ _) = r
    r1 = Env r1' w1' defaultReturn emptyObj
evalClassDec (ClassDec i1 (Just i2) scd1) w r u = evalExp (I i2) (w ! 2) r $ testClass (I i2) $ \(EClass (Class c')) -> u (newEnv i1 (c c'))
  where
    c :: (Ec -> Cc) -> Ev
    c c' = EClass . Class $ \k ->
      c' $ \(EObject (Object o')) ->
        evalSCDec scd1 (w ! 3) (updateEnv (newEnv i1 (c c')) r1) $ \(Object o) ->
          k (EObject (Object (HashMap.union o o')))
    (Env r1' w1' _ _) = r
    r1 = Env r1' w1' defaultReturn emptyObj
evalClassDec d1 _ _ _ = error $ printf "Cannot run evalClassDec with \"%s\"." (pretty d1)

-- | @evalSCDec scdec w r v s@ evaluates the scoped class declaration `scdec` under the environment `r` and with store
-- `s` then passes the resulting environment into the rest of the program `v`.
evalSCDec :: SCDec -> Posn -> Env -> CDc -> Store -> Ans
evalSCDec (Public cd1) w r v = evalCDec cd1 (w ! 1) r $ \o -> v o
evalSCDec (Private cd1) w r v = evalCDec cd1 (w ! 1) r $ \o -> v o
evalSCDec (ChainSCDec scd1 scd2) w r v = evalSCDec scd1 (w ! 1) r $ \(Object o1) -> evalSCDec scd2 (w ! 2) r $
  \(Object o2) -> v (Object (HashMap.union o2 o1))
evalSCDec SkipSCDec w r v = v emptyObj

-- | @evalCDec cdec w r u s@ evaluates the declaration `cdec` as a class declaration under the environment `r` and with
-- store `s` then passes the resulting environment into the rest of the program `u`.
evalCDec :: Dec -> Posn -> Env -> CDc -> Cc
evalCDec (ProcDec i1 i2 _ c1) w r v = v (Object (HashMap.singleton i1 (Dv meth)))
  where
    procd' r c e = evalCom c1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) c
    meth' k o = k $ EProc (procd' (updateThisEnv o r)) (length i2)
    meth = EMethod meth'
evalCDec (FuncDec i1 i2 _ _ e1) w r v = v (Object (HashMap.singleton i1 (Dv meth)))
  where
    func' r k e = evalExp e1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) k
    meth' k o = k $ EFunc (func' (updateThisEnv o r)) (length i2)
    meth = EMethod meth'
evalCDec d1 w r v = evalDec d1 w r (\(Env o _ _ _) -> v (Object o))

-- | @evalNewExp exp w r k s@ evaluates the new expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalNewExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNewExp (New i1) w r k = evalExp (I i1) (w ! 1) r $ testClass (New i1) $ \(EClass (Class c)) -> c k
evalNewExp e1 _ _ _ = error $ printf "Cannot run evalNewExp with \"%s\"." (pretty e1)

-- | @evalThisExp exp w r k s@ evaluates the this expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalThisExp :: Exp -> Posn -> Env -> Ec -> Cc
evalThisExp This w r k = k (EObject this)
  where
    (Env _ _ _ this) = r
evalThisExp e1 _ _ _ = error $ printf "Cannot run evalThisExp with \"%s\"." (pretty e1)

-- | @evalNullExp exp w r k s@ evaluates the null expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalNullExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNullExp Null w r k = k ENull
evalNullExp e1 _ _ _ = error $ printf "Cannot run evalNullExp with \"%s\"." (pretty e1)

-- | A function that returns true iff the value passed in was null.
isNullF :: Function
isNullF k [e] = deref (\e' -> isNull e' ?> (k (EBool True), k (EBool False))) e
isNullF k es = error $ printf "Should only run isNullF with a single parameter, not %d." (length es)

-- | @evalDotExp exp w r k s@ evaluates the dot expression `exp` under the environment `r` and with store `s` then
-- passes the resulting value into the rest of the program `k`.
evalDotExp :: Exp -> Posn -> Env -> Ec -> Cc
evalDotExp (Dot e1 e2) w r k = evalRVal e1 (w ! 1) r $ \e ->
  if
      | isRecord e -> evalExp e2 (w ! 2) (updateEnv (recordToEnv $ evToRecord e) r) k -- Record updates the environment
      | isObject e -> evalExp e2 (w ! 2) (updateEnv (objectToEnv $ evToObject e) r) $ \e' -> case e' of -- Object updates the environment
        (EMethod m) -> m k $ evToObject e -- Methods have "this" passed in
        _ -> k e' -- Anything else passed through
      | otherwise -> err $ printf "\"%s\", evaluated as \"%s\", is not an object or a record." (pretty e1) (pretty e) -- Anything else is an error
evalDotExp e1 _ _ _ = error $ printf "Cannot run evalDotExp with \"%s\"." (pretty e1)