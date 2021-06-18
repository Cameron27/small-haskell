module TypeChecker.Helper.TEnv where

import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control

-- | @updateTEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the
-- return address and current object id from `r` and takes the largest next unique id.
updateTEnv :: TEnv -> TEnv -> TEnv
updateTEnv (TEnv r' cr' _ _ i') (TEnv r cr rt o i) = TEnv (HashMap.union r' r) (HashMap.union cr' cr) rt o (Prelude.max i i')

-- | @updateThisTEnv o r@ returns the environment `r` but with the current object id updated to `o`.
updateThisTEnv :: ObjectId -> TEnv -> TEnv
updateThisTEnv t (TEnv r cr rt _ i) = TEnv r cr rt t i

-- | @lookupTEnv i r@ returns the value that is assigned to `i` in `r`.
lookupTEnv :: Ide -> TEnv -> Either TypeError Type
lookupTEnv i (TEnv r _ _ _ _) = case HashMap.lookup i r of
  Just s -> Right s
  Nothing -> err $ printf "\"%s\" is not defined." i

-- | @lookupClassTEnv o r@ returns the class that is assigned to `o` in `r`.
lookupClassTEnv :: ObjectId -> TEnv -> Either TypeError Class
lookupClassTEnv i (TEnv _ cr _ _ _) = case HashMap.lookup i cr of
  Just s -> Right s
  Nothing -> err $ printf "\"%s\" is not defined." i

-- | @newTEnv i t@ returns a new environment with just `i` bound to `t`.
newTEnv :: Ide -> Type -> TEnv
newTEnv i t = TEnv (HashMap.fromList [(i, t)]) HashMap.empty TVoid emptyObjectId (-1)

-- | @newTEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newTEnvMulti :: [Ide] -> [Type] -> TEnv
newTEnvMulti is ts = TEnv (HashMap.fromList $ reverse $ zip is ts) HashMap.empty TVoid emptyObjectId (-1)

-- | @newClassTEnv i c r@ returns a new environment with just `i` bound to `c` and `c` in the class environment. `r` is
-- used to get the next available unique id.
newClassTEnv :: Ide -> HashMap.HashMap Ide Type -> TEnv -> (TEnv, ObjectId)
newClassTEnv i c (TEnv _ _ _ _ id) = (TEnv (HashMap.fromList [(i, TClass c')]) (HashMap.fromList [(id, c')]) TVoid emptyObjectId (id + 1), id)
  where
    c' = Class id c

-- | An empty type environment.
emptyTEnv :: TEnv
emptyTEnv = TEnv HashMap.empty HashMap.empty TVoid emptyObjectId (-1)

-- | Id of the empty object.
emptyObjectId :: Int
emptyObjectId = -1

-- | An empty class.
emptyClass :: Class
emptyClass = Class emptyObjectId HashMap.empty
