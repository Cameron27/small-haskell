module Interpreter.Core.Dec where

import qualified Data.HashMap.Strict as HashMap
import {-# SOURCE #-} Interpreter.Core.Com
import {-# SOURCE #-} Interpreter.Core.Exp
import Interpreter.Core.Types
import Interpreter.Features.Classes
import Interpreter.Features.Files
import Interpreter.Helper.Array
import Interpreter.Helper.Continuation
import Interpreter.Helper.Env
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Parser.Core.Types

-- | @evalDec dec w r u s@ evaluates the declaration `dec` under the environment `r` and with store `s` then passes the
-- resulting environment into the rest of the program `u`.
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
evalDec (RecordDec i1 is _) w r u s = u (newEnv i1 (ERecord record)) s'
  where
    (ls', s') = newLocsStore (length is) s
    ls = map ELoc ls'
    (Env record' _ _ _) = newEnvMulti is ls
    record = Record record'
evalDec (ProcDec i1 i2 _ c1) w r u s = u (newEnv i1 procd) s
  where
    procd' c e = evalCom c1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) c
    procd = EProc procd' (length i2)
evalDec (RecProcDec i1 i2 _ c1) w r u s = u (newEnv i1 procd) s
  where
    procd = EProc (\c e -> evalCom c1 (w ! 3) (updateEnv (newEnvMulti (i1 : i2) (procd : e)) r) c) (length i2)
evalDec (FuncDec i1 i2 _ _ e1) w r u s = u (newEnv i1 func) s
  where
    func' k e = evalExp e1 (w ! 3) (updateEnv (newEnvMulti i2 e) r) k
    func = EFunc func' (length i2)
evalDec (RecFuncDec i1 i2 _ _ e1) w r u s = u (newEnv i1 func) s
  where
    func = EFunc (\k e -> evalExp e1 (w ! 3) (updateEnv (newEnvMulti (i1 : i2) (func : e)) r) k) (length i2)
evalDec (FileDec i1 i2 t1) w r u s = evalFileDec (FileDec i1 i2 t1) w r u s
evalDec (ClassDec i1 scd1) w r u s = evalClassDec (ClassDec i1 scd1) w r u s
evalDec (ChainDec d1 d2) w r u s = evalDec d1 (w ! 1) r (\r1 -> evalDec d2 (w ! 2) (updateEnv r1 r) (\r2 -> u (updateEnv r2 r1))) s
evalDec SkipDec w r u s = u (Env HashMap.empty HashMap.empty emptyEc emptyObj) s