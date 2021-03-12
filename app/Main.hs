module Main where

import Interpreter.Small (interpretSmall)
import Interpreter.Types (Rv (RInt))
import Parser.Small (parseSmall)
import System.Environment (getArgs)
import System.Exit (die)

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
  let res = interpretSmall parsedProgram [RInt 6, RInt 4, RInt 4, RInt 4, RInt 5]
  print res
