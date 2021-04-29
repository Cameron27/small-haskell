module Interpreter.Core.Dec where

import Interpreter.Core.Types
import Parser.Core.Types

evalDec :: Dec -> Posn -> Env -> Dc -> Cc