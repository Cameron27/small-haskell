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

evalClassDec :: Dec -> Posn -> Env -> Dc -> Cc
evalClassDec (ClassDec i1 d1) w r u = u (newEnv i1 c)
  where
    c = DClass $ Class $ \k -> evalCDec d1 (w ! 2) (updateEnv (newEnv i1 c) r1) $ \(Env r' _ _ _) -> k (DObject $ Object r')
    (Env r1' w1' _ _) = r
    r1 = Env r1' w1' emptyEc emptyObj

evalCDec :: Dec -> Posn -> Env -> Dc -> Cc
evalCDec (ChainDec d1 d2) w r u s = (evalCDec d1 (w ! 1) r $ \r1 -> evalCDec d2 (w ! 2) r $ \r2 -> u (updateEnv r2 r1)) s
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

evalNewExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNewExp (New i1) w r k = evalExp (I i1) (w ! 1) r $ testClass (New i1) $ \(DClass (Class c)) -> c k

evalThisExp :: Exp -> Posn -> Env -> Ec -> Cc
evalThisExp This w r k = k (DObject this)
  where
    (Env _ _ _ this) = r

evalNullExp :: Exp -> Posn -> Env -> Ec -> Cc
evalNullExp Null w r k = k DNull

isNullF :: Function
isNullF k [e] = deref (\e' -> isNull e' ?> (k (DBool True), k (DBool False))) e

evalDotExp :: Exp -> Posn -> Env -> Ec -> Cc
evalDotExp (Dot e1 e2) w r k = evalRVal e1 (w ! 1) r $ \e -> process e
  where
    process r'
      | isRecord r' = evalExp e2 (w ! 2) (updateEnv (recordToEnv $ dvToRecord r') r) k -- Record updates the environment
      | isObject r' = evalExp e2 (w ! 2) (updateEnv (objectToEnv $ dvToObject r') r) $ \e -> case e of -- Object updates the environment
        (DMethod f) -> f k $ dvToObject r' -- Methods have "this" passed in
        _ -> k e -- Anything else passed through
      | otherwise = err $ printf "\"%s\", evaluated as \"%s\", is not an object or a record." (pretty e1) (pretty r') -- Anything else is an error