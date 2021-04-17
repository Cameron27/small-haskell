module TypeChecker.DefaultEnvironment (defaultTEnv) where

import qualified Data.HashMap.Strict as HashMap
import TypeChecker.Types

defaultTEnv :: TEnv
defaultTEnv =
  TEnv
    ( HashMap.fromList
        [ ("reset", TProc [TRef $ TFile TAny]),
          ("rewrite", TProc [TRef $ TFile TAny]),
          ("get", TProc [TRef $ TFile TAny]),
          ("put", TProc [TRef $ TFile TAny]),
          ("eof", TFunc [TRef $ TFile TAny] TBool)
        ]
    )
    TVoid
