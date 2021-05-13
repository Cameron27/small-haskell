module Interpreter.Features.DefaultEnvironment (defaultEnv, defaultEnvAndTEnv) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Types
import Interpreter.Features.Classes
import Interpreter.Features.Files
import Interpreter.Helper.Control
import Parser.Core.Types
import TypeChecker.Core.Types

defaultEnv :: Env
defaultEnv =
  Env
    (HashMap.fromList $ map (\(a, b, _) -> (a, b)) defaultEnvAndTEnv)
    HashMap.empty
    (\e s -> putError "cannot return at top level")
    (Object HashMap.empty)

defaultEnvAndTEnv :: [([Char], Dv, Type)]
defaultEnvAndTEnv =
  [ ("reset", DProc resetFProc 1, TProc [TRef TFileAny]),
    ("rewrite", DProc rewriteFProc 1, TProc [TRef TFileAny]),
    ("get", DProc getFProc 1, TProc [TRef TFileAny]),
    ("put", DProc putFProc 1, TProc [TRef TFileAny]),
    ("eof", DFunc eofFunc 1, TFunc [TRef TFileAny] TBool),
    ("isNull", DFunc isNullF 1, TFunc [TInt] TBool)
  ]
