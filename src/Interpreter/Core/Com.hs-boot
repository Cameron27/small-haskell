module Interpreter.Core.Com where

import Interpreter.Core.Types
import Parser.Core.Types

evalCom :: Com -> Posn -> Env -> Cc -> Cc
