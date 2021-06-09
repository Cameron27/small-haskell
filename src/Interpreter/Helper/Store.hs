module Interpreter.Helper.Store where

import Data.List
import Data.Maybe
import Interpreter.Core.Types
import Text.Printf

-- | @isUnusedStore l s@ returns `True` iff `l` is unused in `s`.
isUnusedStore :: Loc -> Store -> Bool
isUnusedStore l (Store s _) = case s l of
  Unused -> True
  _ -> False

-- | @isUnassignedStore l s@ returns `True` iff `l` is unassigned in `s`.
isUnassignedStore :: Loc -> Store -> Bool
isUnassignedStore l (Store s _) = case s l of
  Unassigned -> True
  Unused -> error $ printf "Tried to check if location \"Loc(%d)\", which is unused, is unassigned." l
  _ -> False

-- | @lookupStore l s@ returns the value that is stored in `s` at `l`.
lookupStore :: Loc -> Store -> Ev
lookupStore l (Store s _) = case s l of
  Sv v -> v
  _ -> error $ printf "Tried to lookup location \"Loc(%d)\" which has no value assigned to it." l

-- | @updateStore l v s@ returns a store with `l` updated to `v`.
updateStore :: Loc -> Maybe Ev -> Store -> Store
updateStore l v (Store s x) = case v of
  Just v -> Store s' x
  Nothing -> Store s'' x
  where
    Just v' = v
    s' l'
      | l == l' = Sv v'
      | otherwise = s l'
    s'' l'
      | l == l' = Unassigned
      | otherwise = s l'

-- | @updateStoreMulti ls vs s@ returns a store with each `l` in `ls` updated to the corresponding `v` in `vs`. If the
-- same `l` appears more than once in `ls` the first occurrence take overrides the later instances.
updateStoreMulti :: [(Loc, Maybe Ev)] -> Store -> Store
updateStoreMulti lvs (Store s x) = Store s' x
  where
    s' l' = case find (\(l, _) -> l == l') lvs of
      Just (_, Just e) -> Sv e
      Just (_, Nothing) -> Unassigned
      _ -> s l'

-- | @newLocStore s@ returns an unused location in `s`.
newLocStore :: Store -> (Loc, Store)
newLocStore (Store s x) = (x, Store s (x + 1))

-- | @newLocsStore n s@ returns an `n` unused location in `s`.
newLocsStore :: Int -> Store -> ([Loc], Store)
newLocsStore n (Store s x) = ([x .. x + n - 1], Store s' (x + n))
  where
    s' i
      | i `elem` [x .. x + n - 1] = Unassigned
      | otherwise = s i

-- | The store where every location is unassigned.
unusedStore :: b -> StoreVal
unusedStore = const Unused