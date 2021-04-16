module TypeChecker.Helper.TEnv where

import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import TypeChecker.Helper.Control
import TypeChecker.Types

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed.
updateTEnv :: TEnv -> TEnv -> TEnv
updateTEnv (TEnv r' _) (TEnv r rt) = TEnv (HashMap.union r' r) rt

-- | @lookupTEnv i r@ returns the value that is assigned to `i` in `r`.
lookupTEnv :: Ide -> TEnv -> Either TypeError Type
lookupTEnv i (TEnv r _) = case HashMap.lookup i r of
  Just s -> Right s
  Nothing -> err $ printf "\"%s\" is not defined" i

-- | @newTEnv i t@ returns a new environment with just `i` bound to `t`.
newTEnv :: Ide -> Type -> TEnv
newTEnv i t = TEnv (HashMap.fromList [(i, t)]) TVoid

-- | @newTEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newTEnvMulti :: [Ide] -> [Type] -> TEnv
newTEnvMulti is ts = TEnv (HashMap.fromList $ reverse $ zip is ts) TVoid

emptyTEnv :: TEnv
emptyTEnv = TEnv HashMap.empty TVoid