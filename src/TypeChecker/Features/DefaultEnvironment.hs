module TypeChecker.Features.DefaultEnvironment (defaultTEnv) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Features.DefaultEnvironment
import TypeChecker.Core.Types
import TypeChecker.Helper.TEnv

-- | The starting environment to type check a small program under.
defaultTEnv :: TEnv
defaultTEnv =
  TEnv
    (HashMap.fromList $ map (\(a, _, c) -> (a, c)) defaultEnvAndTEnv ++ [("", TClass emptyClass)])
    (HashMap.fromList [(-1, Class (-1) HashMap.empty)])
    TVoid
    emptyObjectId
    (emptyObjectId + 1)
