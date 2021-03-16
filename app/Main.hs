module Main where

import Interpreter.Small
import Interpreter.Types
import Parser.Small
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  filePath <- case args of
    (x : _) -> return x
    [] -> die "Usage: small-haskell FILE"
  program <- readFile filePath
  let parsedProgram' = parseSmall filePath program
  parsedProgram <- case parsedProgram' of
    Right p -> return p
    Left err -> die $show err
  exitCode <- interpretSmall parsedProgram
  exitWith exitCode
