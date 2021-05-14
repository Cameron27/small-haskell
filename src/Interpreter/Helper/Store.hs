module Interpreter.Helper.Store where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Interpreter.Core.Types
import Text.Printf

-- | @isUnused l s@ returns `True` iff `l` is unused in `s`.
isUnusedStore :: Loc -> Store -> Bool
isUnusedStore l (Store s _) = isNothing $ HashMap.lookup l s

-- | @lookupStore l s@ returns the value that is stored in `s` at `l`.
lookupStore :: Loc -> Store -> Dv
lookupStore l (Store s _) = case HashMap.lookup l s of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup location \"Loc(%d)\" which has no value assigned to it." l

-- | @updateStore l v s@ returns a store with `l` updated to `v`.
updateStore :: Loc -> Maybe Dv -> Store -> Store
updateStore l v (Store s x) = case v of
  Just v -> Store (HashMap.insert l v s) x
  Nothing -> Store (HashMap.delete l s) x

-- | @updateStoreMulti ls vs s@ returns a store with each `l` in `ls` updated tp the corresponding `v` in `vs`. If the
-- same `l` appears more than once in `ls` the first occurrence take overrides the later instances.
updateStoreMulti :: [Loc] -> [Maybe Dv] -> Store -> Store
updateStoreMulti ls vs s = foldl (\s (l, v) -> updateStore l v s) s $ reverse $ zip ls vs

-- | @newLocStore s@ returns an unused location in `s`.
newLocStore :: Store -> (Loc, Store)
newLocStore (Store s x) = (x, Store s (x + 1))

-- | @newLocsStore n s@ returns an `n` unused location in `s`.
newLocsStore :: Int -> Store -> ([Loc], Store)
newLocsStore n (Store s x) = ([x .. x + n - 1], Store s (x + n))