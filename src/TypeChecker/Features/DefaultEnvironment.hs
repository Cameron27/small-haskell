module TypeChecker.Features.DefaultEnvironment (defaultTEnv) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Features.DefaultEnvironment
import TypeChecker.Core.Types
import TypeChecker.Helper.TEnv

defaultTEnv :: TEnv
defaultTEnv =
  TEnv
    (HashMap.fromList $ map (\(a, _, c) -> (a, c)) defaultEnvAndTEnv ++ [("", TClass emptyClass)])
    TVoid
    (Class "" HashMap.empty)
