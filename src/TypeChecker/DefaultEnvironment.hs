module TypeChecker.DefaultEnvironment (defaultTEnv) where

import qualified Data.HashMap.Strict as HashMap
import TypeChecker.Types

defaultTEnv :: TEnv
defaultTEnv =
  TEnv
    ( HashMap.fromList
        [ ("reset", TProc [TRef TFileAny]),
          ("rewrite", TProc [TRef TFileAny]),
          ("get", TProc [TRef TFileAny]),
          ("put", TProc [TRef TFileAny]),
          ("eof", TFunc [TRef TFileAny] TBool)
        ]
    )
    TVoid
