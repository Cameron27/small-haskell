module Interpreter.Helper.Continuation where

import Common.Formatting
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Text.Printf

-- | @cont k e s@ looks up the location `e` in `s` and passes the result, along with `s` into `k`.
cont :: Ec -> Ec
cont k e s =
  isLoc e
    ?> ( isUnusedStore l s ?> (putError $ printf "\"Loc(%d)\" is unbound." l, k d s),
         error $ printf "\"%s\" is not a location." (pretty e)
       )
  where
    l = evToLoc e
    d = svToDv $ lookupStore l s

-- | @update l c e s@ stores `e` at `l` and passes the resulting store to `c`.
update :: Loc -> Cc -> Ec
update l c e s = isSv e ?> (c (updateStore l (Just $ evToSv e) s), putError $ printf "tried to store the value \"%s\" which is not storable." (pretty e))

-- | @ref k e s@ gets an unused location in `s`, updates it with `e` then passes it, along with the updated store, to
-- `k`.
ref :: Ec -> Ec
ref k e s = update newLoc (k (DLoc newLoc)) e s'
  where
    (newLoc, s') = newLocStore s

-- | @deref k e s@ checks if `e` is a location. If it is then the content of `e`, along with `s`, are passed to `k`,
-- otherwise it just passes `e` and `s` to `k`.
deref :: Ec -> Ec
deref k e s = isLoc e ?> (cont k e s, k e s)