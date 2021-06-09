module Interpreter.Features.DefaultEnvironment (defaultEnv, defaultEnvAndTEnv) where

import Data.List
import Interpreter.Core.Types
import Interpreter.Features.Classes
import Interpreter.Features.Files
import Interpreter.Helper.Env
import TypeChecker.Core.Types

-- | The starting environment to run a small program under.
defaultEnv :: Env
defaultEnv =
  Env
    (fromListEnv $ map (\(a, b, _) -> (a, b)) defaultEnvAndTEnv)
    unboundEnv
    defaultReturn
    emptyObj

-- | The names, values and types of the staring environment of small.
defaultEnvAndTEnv :: [([Char], Ev, Type)]
defaultEnvAndTEnv =
  [ ("reset", EProc resetFProc 1, TProc [TRef TFileAny]),
    ("rewrite", EProc rewriteFProc 1, TProc [TRef TFileAny]),
    ("get", EProc getFProc 1, TProc [TRef TFileAny]),
    ("put", EProc putFProc 1, TProc [TRef TFileAny]),
    ("eof", EFunc eofFunc 1, TFunc [TRef TFileAny] TBool),
    ("isNull", EFunc isNullF 1, TFunc [TUnion [TObjectAny, TRef TObjectAny]] TBool)
  ]
