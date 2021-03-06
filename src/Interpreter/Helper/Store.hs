module Interpreter.Helper.Store where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Interpreter.Core.Types
import Text.Printf

-- | @lookupStore l s@ returns the value that is stored in `s` at `l`.
lookupStore :: Loc -> Store -> StoreVal
lookupStore l (Store s _) = fromMaybe Unused (HashMap.lookup l s)

-- | @updateStore l v s@ returns a store with `l` updated to `v`.
updateStore :: Loc -> Maybe Ev -> Store -> Store
updateStore l v (Store s x) = case v of
  Just v -> Store (HashMap.insert l (Sv v) s) x
  Nothing -> Store (HashMap.insert l Unassigned s) x

-- | @updateStoreMulti ls vs s@ returns a store with each `l` in `ls` updated tp the corresponding `v` in `vs`. If the
-- same `l` appears more than once in `ls` the first occurrence take overrides the later instances.
updateStoreMulti :: [Loc] -> [Maybe Ev] -> Store -> Store
updateStoreMulti ls vs s = foldl (\s (l, v) -> updateStore l v s) s $ reverse $ zip ls vs

-- | @newLocStore s@ returns an unused location in `s`.
newLocStore :: Store -> (Loc, Store)
newLocStore (Store s x) = (x, Store s (x + 1))

-- | @newLocsStore n s@ returns an `n` unused location in `s`.
newLocsStore :: Int -> Store -> ([Loc], Store)
newLocsStore n (Store s x) = (ls, s')
  where
    ls = [x .. x + n - 1]
    nothingList = Nothing : nothingList
    s' = updateStoreMulti ls nothingList (Store s (x + n))