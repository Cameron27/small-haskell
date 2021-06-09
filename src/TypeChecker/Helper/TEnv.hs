module TypeChecker.Helper.TEnv where

import Data.List
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control

-- | @updateTEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the
-- return address and current class from `r` and takes the largest next unique id.
updateTEnv :: TEnv -> TEnv -> TEnv
updateTEnv (TEnv r' cr' _ _ i') (TEnv r cr rt c i) = TEnv (unionTEnv r' r) (unionTEnv cr' cr) rt c (Prelude.max i i')

-- | @updateThisTEnv c r@ returns the environment `r` but with the current class updated to `t`.
updateThisTEnv :: Class -> TEnv -> TEnv
updateThisTEnv t (TEnv r cr rt _ i) = TEnv r cr rt t i

-- | @lookupTEnv i r@ returns the value that is assigned to `i` in `r`.
lookupTEnv :: Ide -> TEnv -> Either TypeError Type
lookupTEnv i (TEnv r _ _ _ _) = case r i of
  T s -> Right s
  _ -> err $ printf "\"%s\" is not defined" i

-- | @lookupClassTEnv i r@ returns the class that is assigned to `i` in `r`.
lookupClassTEnv :: Int -> TEnv -> Either TypeError Class
lookupClassTEnv i (TEnv _ cr _ _ _) = case cr i of
  T (TClass s) -> Right s
  Unbound -> err $ printf "\"%s\" is not defined" i
  _ -> error $ printf "Tried to lookup class \"%d\" which has no value assigned to it." i

-- | @newTEnv i t@ returns a new environment with just `i` bound to `t`.
newTEnv :: Ide -> Type -> TEnv
newTEnv i t = TEnv (fromValTEnv i t) unboundTEnv TVoid emptyClass (-1)

-- | @newTEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newTEnvMulti :: [Ide] -> [Type] -> TEnv
newTEnvMulti is ts = TEnv (fromListTEnv $ zip is ts) unboundTEnv TVoid emptyClass (-1)

-- | @newTEnv i c r@ returns a new environment with just `i` bound to `c` and `c` in the class environment. `r` is used
-- to get the next available unique id.
newClassTEnv :: Ide -> (Ide -> TEnvVal) -> TEnv -> (TEnv, Class)
newClassTEnv i c (TEnv _ _ _ _ id) = (TEnv (fromValTEnv i $ TClass c') (fromValTEnv id $ TClass c') TVoid emptyClass (id + 1), c')
  where
    c' = Class id c

-- | An empty type environment.
emptyTEnv :: TEnv
emptyTEnv = TEnv unboundTEnv unboundTEnv TVoid emptyClass (-1)

-- | An empty class.
emptyClass :: Class
emptyClass = Class (-1) unboundTEnv

-- | @fromValTEnv i t@ returns a type environment map with `i` bound to `t` and everything else unbound.
fromValTEnv :: Eq a => a -> Type -> a -> TEnvVal
fromValTEnv i e i'
  | i == i' = T e
  | otherwise = Unbound

-- | @envFromList l@ returns an type environment map with the identifiers in `l` bound to the types in `l` and
-- everything else unbound.
fromListTEnv :: Eq a => [(a, Type)] -> a -> TEnvVal
fromListTEnv l i = case find (\(i', _) -> i == i') l of
  Just (_, e) -> T e
  _ -> Unbound

-- | @envFromList r' r@ returns an type environment map that checks @r'@ then `r`.
unionTEnv :: (a -> TEnvVal) -> (a -> TEnvVal) -> a -> TEnvVal
unionTEnv r' r i = case r' i of
  T e -> T e
  _ -> r i

-- | The environment map where every id is unbound.
unboundTEnv :: b -> TEnvVal
unboundTEnv = const Unbound
