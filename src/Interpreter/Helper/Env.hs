module Interpreter.Helper.Env where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Interpreter.Types
import Text.Printf

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the return
-- address from `r`.
updateEnv :: Env -> Env -> Env
updateEnv (Env r' w' _) (Env r w k) = Env (HashMap.union r' r) (HashMap.union w' w) k

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> Dv
lookupEnv i (Env r _ _) = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it.\n%s" i (show r)

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnvOwn :: Int -> Env -> Dv
lookupEnvOwn i (Env _ w _) = case HashMap.lookup i w of
  Just s -> s
  _ -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it.\n%s" (show i) (show w)

-- | @newEnv i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Dv -> Env
newEnv i l = Env (HashMap.fromList [(i, l)]) HashMap.empty emptyEc

-- | @newEnvOwn i d@ returns a new environment with just `i` bound to `d`.
newEnvOwn :: Int -> Dv -> Env
newEnvOwn i l = Env HashMap.empty (HashMap.fromList [(i, l)]) emptyEc

-- | @newEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Dv] -> Env
newEnvMulti is ls = Env (HashMap.fromList $ reverse $ zip is ls) HashMap.empty emptyEc

-- | @isUnboundEnv i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnv :: Ide -> Env -> Bool
isUnboundEnv i (Env r _ _) = case HashMap.lookup i r of
  Just _ -> False
  Nothing -> True

-- | @isUnboundEnv i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnvOwn :: Int -> Env -> Bool
isUnboundEnvOwn i (Env _ w _) = case HashMap.lookup i w of
  Just _ -> False
  _ -> True

emptyEc :: Ec
emptyEc _ _ = error "This should not be the return address."

emptyEnv :: Env
emptyEnv = Env HashMap.empty HashMap.empty emptyEc