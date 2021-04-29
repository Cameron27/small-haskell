module Interpreter.Small (interpretSmall) where

import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Com
import Interpreter.Core.Types
import Interpreter.Features.DefaultEnvironment
import Interpreter.Features.Own
import Interpreter.Helper.Env
import Parser.Core.Types
import System.Exit

evalPgm :: Pgm -> Ans
evalPgm (Program c) = do
  evalOwnCom c w env (\r -> evalCom c w (updateEnv r env) (\_ -> return ExitSuccess)) store
  where
    env = defaultEnv
    store = Store HashMap.empty 0
    w = Posn 1

interpretSmall :: Pgm -> Ans
interpretSmall = evalPgm