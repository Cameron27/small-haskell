module Interpreter.Helper.Array where

import Common.Formatting
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Text.Printf

-- | @newArray (i1, i2) k s@ creates a new array with store `s` using `i1` and `i2` as the bounds then passes the
-- resulting value into the rest of the program `k`.
newArray :: (Int, Int) -> Ec -> Cc
newArray (n1, n2) k s =
  (n1 > n2)
    ?> ( putError $ printf "the array bounds [%d:%d] are invalid." n1 n2,
         k (EArray $ Array n1 n2 locs) s'
       )
  where
    (locs, s') = newLocsStore (n2 - n1 + 1) s

-- | @arrayAccess a k e s@ gets the value of index `e` in array `a` with store `s` then passes the resulting value into
-- the rest of the program `k`.
arrayAccess :: Array -> Ec -> Ec
arrayAccess (Array n1 n2 ls) k e =
  isInt e
    ?> ( (n >= n1 && n <= n2)
           ?> ( k $ ELoc (ls !! fromIntegral (n - n1)),
                err $ printf "array index %d is outside of range %d to %d" n n1 n2
              ),
         err $ printf "array index \"%s\" is not an integer" (pretty e)
       )
  where
    n = evToInt e