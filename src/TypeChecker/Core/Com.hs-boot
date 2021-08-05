module TypeChecker.Core.Com where

import Parser.Core.Types
import TypeChecker.Core.Types

typeCom :: Com -> TEnv -> TypeResult ()