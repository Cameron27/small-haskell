module Interpreter.Helper.Env where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Text.Printf

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the return
-- address and current object from `r`.
updateEnv :: Env -> Env -> Env
updateEnv (Env r' w' _ _) (Env r w k t) = Env (HashMap.union r' r) (HashMap.union w' w) k t

-- | @updateThisEnv t r@ returns the environment `r` but with the current object updated to `t`.
updateThisEnv :: Object -> Env -> Env
updateThisEnv t (Env r w k _) = Env r w k t

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> EnvVal
lookupEnv i (Env r _ _ _) = fromMaybe Unbound (HashMap.lookup i r)

-- | @lookupEnv (i, w) r@ returns the value that is assigned to `(i, w)` in `r`.
lookupEnvOwn :: (Ide, Posn) -> Env -> EnvVal
lookupEnvOwn i (Env _ w _ _) = fromMaybe Unbound (HashMap.lookup i w)

-- | @newEnv i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Ev -> Env
newEnv i d = Env (HashMap.fromList [(i, Dv d)]) HashMap.empty emptyEc emptyObj

-- | @newEnvOwn (i, w) d@ returns a new environment with just `(i, w)` bound to `d`.
newEnvOwn :: (Ide, Posn) -> Ev -> Env
newEnvOwn i d = Env HashMap.empty (HashMap.fromList [(i, Dv d)]) emptyEc emptyObj

-- | @newEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Ev] -> Env
newEnvMulti is ds = Env (HashMap.fromList $ reverse $ zip is (map Dv ds)) HashMap.empty emptyEc emptyObj

-- | The empty return address.
emptyEc :: Ec
emptyEc _ _ = error "This should not be the return address."

-- | Default return address that produces an error.
defaultReturn :: Ev -> Store -> Ans
defaultReturn e s = putError "no return address"

-- | The empty object.
emptyObj :: Object
emptyObj = Object HashMap.empty HashMap.empty (-1)

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env HashMap.empty HashMap.empty emptyEc emptyObj
