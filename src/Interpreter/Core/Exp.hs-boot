module Interpreter.Core.Exp where

import Interpreter.Core.Types
import Parser.Core.Types

evalRVal :: Exp -> Posn -> Env -> Ec -> Cc
evalExp :: Exp -> Posn -> Env -> Ec -> Cc