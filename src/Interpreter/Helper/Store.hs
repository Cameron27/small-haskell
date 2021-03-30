module Interpreter.Helper.Store where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Interpreter.Types
import Text.Printf

-- | @isUnused l s@ returns `True` iff the `l` is unused in `s`.
isUnusedStore :: Loc -> Store -> Bool
isUnusedStore l (Store s _) = isNothing $ HashMap.lookup l s

-- | @lookupStore l s@ returns the value that is stored in `s` at `l`.
lookupStore :: Loc -> Store -> Sv
lookupStore l (Store s _) = case HashMap.lookup l s of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup location \"Loc(%d)\" which has no value assigned to it." l

-- | @updateStore l v s@ returns a store with `l` updated to `v`.
updateStore :: Loc -> Sv -> Store -> Store
updateStore l v (Store s x) = Store (HashMap.insert l v s) x

-- | @newLocStore s@ returns an unused location in `s`.
newLocStore :: Store -> (Loc, Store)
newLocStore (Store s x) = (x, Store s (x + 1))

-- | @newLocsStore n s@ returns an `n` unused location in `s`.
newLocsStore :: Integer -> Store -> ([Loc], Store)
newLocsStore n (Store s x) = ([x .. x + n - 1], Store s (x + n))