module Interpreter.Helper.Env where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Types
import Text.Printf

-- | @updateE r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed.
updateEnv :: Env -> Env -> Env
updateEnv = HashMap.union

-- | @lookupE i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> Dv
lookupEnv i r = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it." i

-- | @newE i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Dv -> Env
newEnv i l = HashMap.fromList [(i, l)]

-- | @newE is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the same
-- `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Dv] -> Env
newEnvMulti is ls = HashMap.fromList $ reverse $ zip is ls

-- | @isUnboundE i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnv :: Ide -> Env -> Bool
isUnboundEnv i r = case HashMap.lookup i r of
  Just _ -> False
  Nothing -> True