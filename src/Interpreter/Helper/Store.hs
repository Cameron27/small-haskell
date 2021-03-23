module Interpreter.Helper.Store where

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Interpreter.Types
import Text.Printf

-- | @isUnused l s@ returns `True` iff the `l` is unused in `s`.
isUnusedStore :: Loc -> Store -> Bool
isUnusedStore l (Store s _) = isNothing $ HashMap.lookup l s

-- | @lookupS l s@ returns the value that is stored in `s` at `l`.
lookupStore :: Loc -> Store -> Sv
lookupStore l (Store s _) = case HashMap.lookup l s of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup location \"Loc(%d)\" which has no value assigned to it." l

-- | @updateS l v s@ returns a store with `l` updated to `v`.
updateStore :: Loc -> Sv -> Store -> Store
updateStore l v (Store s x) = Store (HashMap.insert l v s) x

-- | @newLocS s@ returns an unused location in s.
newLocStore :: Store -> (Loc, Store)
newLocStore (Store s x) = (x, Store s (x + 1))