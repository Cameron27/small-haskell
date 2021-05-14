module TypeChecker.Small (typeCheckSmall) where

import Parser.Core.Types
import TypeChecker.Core.Com
import TypeChecker.Core.Types
import TypeChecker.Features.DefaultEnvironment

-- | @typePgm p@ type checks program `p`.
typePgm :: Pgm -> Either TypeError ()
typePgm (Program c) = do typeCom c defaultTEnv; return ()

-- | @typeCheckSmall p@ type checks program `p` returning an error if one is encountered.
typeCheckSmall :: Pgm -> Maybe TypeError
typeCheckSmall p = case typePgm p of
  Right _ -> Nothing
  Left err -> Just err