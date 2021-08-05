module TypeChecker.Helper.Control where

import TypeChecker.Core.Types

-- | @err s@ returns a `TypeError` with message `s`.
err :: String -> TypeResult b
err s = Left $ TypeError s