module Interpreter.Features.DefaultEnvironment (defaultEnv, defaultEnvAndTEnv) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Types
import Interpreter.Features.Classes
import Interpreter.Features.Files
import Interpreter.Helper.Control
import Parser.Core.Types
import TypeChecker.Core.Types

-- | The starting environment to run a small program under.
defaultEnv :: Env
defaultEnv =
  Env
    (HashMap.fromList $ map (\(a, b, _) -> (a, b)) defaultEnvAndTEnv)
    HashMap.empty
    (\e s -> putError "cannot return at top level")
    (Object HashMap.empty)

-- | The names, values and types of the staring environment of small.
defaultEnvAndTEnv :: [([Char], Dv, Type)]
defaultEnvAndTEnv =
  [ ("reset", DProc resetFProc 1, TProc [TRef TFileAny]),
    ("rewrite", DProc rewriteFProc 1, TProc [TRef TFileAny]),
    ("get", DProc getFProc 1, TProc [TRef TFileAny]),
    ("put", DProc putFProc 1, TProc [TRef TFileAny]),
    ("eof", DFunc eofFunc 1, TFunc [TRef TFileAny] TBool),
    ("isNull", DFunc isNullF 1, TFunc [TUnion [TObjectAny, TRef TObjectAny]] TBool)
  ]
