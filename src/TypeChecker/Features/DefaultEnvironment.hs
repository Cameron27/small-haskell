module TypeChecker.Features.DefaultEnvironment (defaultTEnv) where

import Data.List
import Interpreter.Features.DefaultEnvironment
import TypeChecker.Core.Types
import TypeChecker.Helper.TEnv

-- | The starting environment to type check a small program under.
defaultTEnv :: TEnv
defaultTEnv =
  TEnv
    (fromListTEnv $ map (\(a, _, c) -> (a, c)) defaultEnvAndTEnv ++ [("", TClass emptyClass)])
    (fromValTEnv (-1) $ TClass $ Class (-1) unboundTEnv)
    TVoid
    (Class (-1) unboundTEnv)
    0
