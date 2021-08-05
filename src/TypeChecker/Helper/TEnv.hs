module TypeChecker.Helper.TEnv where

import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control

-- | @updateTEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the
-- return address and current class id from `r` and takes the largest next unique id.
updateTEnv :: TEnv -> TEnv -> TEnv
updateTEnv (TEnv r' c' _ _ i') (TEnv r c rt o i) = TEnv (HashMap.union r' r) (HashMap.union c' c) rt o (Prelude.max i i')

-- | @updateThisTEnv o r@ returns the environment `r` but with the current class id updated to `o`.
updateThisTEnv :: ClassId -> TEnv -> TEnv
updateThisTEnv t (TEnv r c rt _ i) = TEnv r c rt t i

-- | @lookupTEnv i r@ returns the value that is assigned to `i` in `r`.
lookupTEnv :: Ide -> TEnv -> TypeResult Type
lookupTEnv i (TEnv r _ _ _ _) = case HashMap.lookup i r of
  Just s -> Right s
  Nothing -> err $ printf "\"%s\" is not defined." i

-- | @lookupClassTEnv o r@ returns the class that is assigned to `o` in `r`.
lookupClassTEnv :: ClassId -> TEnv -> TypeResult Class
lookupClassTEnv i (TEnv _ c _ _ _) = case HashMap.lookup i c of
  Just s -> Right s
  Nothing -> err $ printf "\"%s\" is not defined." i

-- | @newTEnv i t@ returns a new environment with just `i` bound to `t`.
newTEnv :: Ide -> Type -> TEnv
newTEnv i t = TEnv (HashMap.fromList [(i, t)]) HashMap.empty TVoid emptyClassId (-1)

-- | @newTEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newTEnvMulti :: [Ide] -> [Type] -> TEnv
newTEnvMulti is ts = TEnv (HashMap.fromList $ reverse $ zip is ts) HashMap.empty TVoid emptyClassId (-1)

-- | @newClassTEnv i id c1 c2 r@ returns a new environment with just `i` bound to a new class with public type map `c1`,
-- private type map `c2` and parent id `id`. `r` is used to get the next available unique id.
newClassTEnv :: Ide -> ClassId -> TypeMap -> TypeMap -> TEnv -> (TEnv, ClassId)
newClassTEnv i id2 c1 c2 (TEnv _ _ _ _ id) = (TEnv (HashMap.fromList [(i, TClass c')]) (HashMap.fromList [(id, c')]) TVoid emptyClassId (id + 1), id)
  where
    c' = Class id id2 c1 c2

-- | An empty type environment.
emptyTEnv :: TEnv
emptyTEnv = TEnv HashMap.empty HashMap.empty TVoid emptyClassId (-1)

-- | An empty class.
emptyClass :: Class
emptyClass = Class emptyClassId emptyClassId HashMap.empty HashMap.empty
