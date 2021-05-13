module Interpreter.Helper.Array where

import Common.Formatting
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Text.Printf

newArray :: (Int, Int) -> Ec -> Cc
newArray (n1, n2) k s =
  (n1 > n2)
    ?> ( putError $ printf "the array bounds [%d:%d] are invalid." n1 n2,
         k (DArray $ Array n1 n2 locs) s'
       )
  where
    (locs, s') = newLocsStore (n2 - n1 + 1) s

arrayAccess :: Array -> Ec -> Ec
arrayAccess (Array n1 n2 ls) k e =
  isInt e
    ?> ( (n >= n1 && n <= n2)
           ?> ( k $ DLoc (ls !! fromIntegral (n - n1)),
                err $ printf "array index %d is outside of range %d to %d" n n1 n2
              ),
         err $ printf "array index %s is not an integer" (pretty e)
       )
  where
    n = dvToInt e