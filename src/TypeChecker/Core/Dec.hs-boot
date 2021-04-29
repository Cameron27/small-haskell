module TypeChecker.Core.Dec where

import Parser.Core.Types
import TypeChecker.Core.Types

typeDec :: Dec -> TEnv -> Either TypeError TEnv