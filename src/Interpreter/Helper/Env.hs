module Interpreter.Helper.Env where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Types
import Text.Printf

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the return
-- address from `r`.
updateEnv :: Env -> Env -> Env
updateEnv (Env r' _) (Env r k) = Env (HashMap.union r' r) k

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> Dv
lookupEnv i (Env r _) = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it." i

-- | @newEnv i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Dv -> Env
newEnv i l = Env (HashMap.fromList [(i, l)]) emptyEc

-- | @newEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Dv] -> Env
newEnvMulti is ls = Env (HashMap.fromList $ reverse $ zip is ls) emptyEc

-- | @isUnboundEnv i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnv :: Ide -> Env -> Bool
isUnboundEnv i (Env r _) = case HashMap.lookup i r of
  Just _ -> False
  Nothing -> True

emptyEc :: Ec
emptyEc _ _ = error "This should not be the return address."