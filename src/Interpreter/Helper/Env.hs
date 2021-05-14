module Interpreter.Helper.Env where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Types
import Text.Printf

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the return
-- address and current object from `r`.
updateEnv :: Env -> Env -> Env
updateEnv (Env r' w' _ _) (Env r w k t) = Env (HashMap.union r' r) (HashMap.union w' w) k t

-- | @updateThisEnv t r@ returns the environment `r` but with the current object updated to `t`.
updateThisEnv :: Object -> Env -> Env
updateThisEnv t (Env r w k _) = Env r w emptyEc t

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> Dv
lookupEnv i (Env r _ _ _) = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it.\n%s" i (pretty r)

-- | @lookupEnv (i, w) r@ returns the value that is assigned to `(i, w)` in `r`.
lookupEnvOwn :: (Ide, Posn) -> Env -> Dv
lookupEnvOwn i (Env _ w _ _) = case HashMap.lookup i w of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"(%s, %s)\" which has no value assigned to it.\n%s" (fst i) (pretty $ snd i) (pretty w)

-- | @newEnv i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Dv -> Env
newEnv i l = Env (HashMap.fromList [(i, l)]) HashMap.empty emptyEc emptyObj

-- | @newEnvOwn (i, w) d@ returns a new environment with just `(i, w)` bound to `d`.
newEnvOwn :: (Ide, Posn) -> Dv -> Env
newEnvOwn i l = Env HashMap.empty (HashMap.fromList [(i, l)]) emptyEc emptyObj

-- | @newEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Dv] -> Env
newEnvMulti is ls = Env (HashMap.fromList $ reverse $ zip is ls) HashMap.empty emptyEc emptyObj

-- | @isUnboundEnv i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnv :: Ide -> Env -> Bool
isUnboundEnv i (Env r _ _ _) = case HashMap.lookup i r of
  Just _ -> False
  Nothing -> True

-- | The empty return address.
emptyEc :: Ec
emptyEc _ _ = error "This should not be the return address."

-- | The empty object.
emptyObj :: Object
emptyObj = Object HashMap.empty

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env HashMap.empty HashMap.empty emptyEc emptyObj