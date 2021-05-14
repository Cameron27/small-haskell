module TypeChecker.Helper.Control where

import TypeChecker.Core.Types

-- | @err s@ returns a `TypeError` with message `s`.
err :: String -> Either TypeError b
err s = Left $ TypeError s