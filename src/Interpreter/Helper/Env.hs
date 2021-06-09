module Interpreter.Helper.Env where

import Common.Formatting
import Data.List
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Text.Printf

-- | @updateEnv r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed. Keeps the return
-- address and current object from `r`.
updateEnv :: Env -> Env -> Env
updateEnv (Env r' w' _ _) (Env r w k t) = Env (unionEnv r' r) (unionEnv w' w) k t

-- | @updateThisEnv t r@ returns the environment `r` but with the current object updated to `t`.
updateThisEnv :: Object -> Env -> Env
updateThisEnv t (Env r w k _) = Env r w k t

-- | @lookupEnv i r@ returns the value that is assigned to `i` in `r`.
lookupEnv :: Ide -> Env -> Ev
lookupEnv i (Env r _ _ _) = case r i of
  Dv s -> s
  _ -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it." i

-- | @lookupEnv (i, w) r@ returns the value that is assigned to `(i, w)` in `r`.
lookupEnvOwn :: (Ide, Posn) -> Env -> Ev
lookupEnvOwn i (Env _ w _ _) = case w i of
  Dv s -> s
  _ -> error $ printf "Tried to lookup identifier \"(%s, %s)\" which has no value assigned to it." (fst i) (pretty $ snd i)

-- | @newEnv i d@ returns a new environment with just `i` bound to `d`.
newEnv :: Ide -> Ev -> Env
newEnv i l = Env (fromValEnv i l) unboundEnv emptyEc emptyObj

-- | @newEnvOwn (i, w) d@ returns a new environment with just `(i, w)` bound to `d`.
newEnvOwn :: (Ide, Posn) -> Ev -> Env
newEnvOwn i l = Env unboundEnv (fromValEnv i l) emptyEc emptyObj

-- | @newEnvMulti is ds@ returns a new environment with each `i` in `is` bound to the corresponding `d` in `ds`. If the
-- same `i` appears more than once in `is` the first occurrence take overrides the later instances.
newEnvMulti :: [Ide] -> [Ev] -> Env
newEnvMulti is ls = Env (fromListEnv $ zip is ls) unboundEnv emptyEc emptyObj

-- | @isUnboundEnv i r@ is `True` iff `i` is unbound in `r`.
isUnboundEnv :: Ide -> Env -> Bool
isUnboundEnv i (Env r _ _ _) = case r i of
  Dv _ -> False
  _ -> True

-- | The empty return address.
emptyEc :: Ec
emptyEc _ _ = error "This should not be the return address."

-- | Default return address that produces an error.
defaultReturn :: Ev -> Store -> Ans
defaultReturn e s = putError "no return address"

-- | The empty object.
emptyObj :: Object
emptyObj = Object unboundEnv unboundEnv (-1)

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env unboundEnv unboundEnv emptyEc emptyObj

-- | @fromValEnv i e@ returns an environment map with `i` bound to `e` and everything else unbound.
fromValEnv :: Eq a => a -> Ev -> a -> EnvVal
fromValEnv i e i'
  | i == i' = Dv e
  | otherwise = Unbound

-- | @fromListEnv l@ returns an environment map with the identifiers in `l` bound to the values in `l` and everything
-- else unbound.
fromListEnv :: Eq a => [(a, Ev)] -> a -> EnvVal
fromListEnv l i = case find (\(i', _) -> i == i') l of
  Just (_, e) -> Dv e
  _ -> Unbound

-- | @fromListEnv r' r@ returns an environment map that checks @r'@ then `r`.
unionEnv :: (a -> EnvVal) -> (a -> EnvVal) -> a -> EnvVal
unionEnv r' r i = case r' i of
  Dv e -> Dv e
  _ -> r i

-- | The environment map where every id is unbound.
unboundEnv :: b -> EnvVal
unboundEnv = const Unbound