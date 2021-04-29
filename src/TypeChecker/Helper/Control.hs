module TypeChecker.Helper.Control where

import TypeChecker.Core.Types

err :: String -> Either TypeError b
err s = Left $ TypeError s