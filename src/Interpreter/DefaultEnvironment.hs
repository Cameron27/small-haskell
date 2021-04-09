module Interpreter.DefaultEnvironment where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.FileOperations
import Interpreter.Helper.Control
import Interpreter.Types
import Parser.Types

defaultEnv :: Env
defaultEnv =
  Env
    ( HashMap.fromList
        [ ("reset", DProc resetFProc 1),
          ("rewrite", DProc rewriteFProc 1),
          ("get", DProc getFProc 1),
          ("put", DProc putFProc 1),
          ("eof", DFunc eofFunc 1)
        ]
    )
    HashMap.empty
    (\e s -> putError "cannot return at top level")
