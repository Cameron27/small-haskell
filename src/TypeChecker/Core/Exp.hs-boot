module TypeChecker.Core.Exp where

import Parser.Core.Types
import TypeChecker.Core.Types

typeExp :: Exp -> TEnv -> Either TypeError Type