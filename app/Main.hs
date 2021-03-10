module Main where

import SmallParser
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  filePath <- case args of
    (x : _) -> return x
    [] -> die "Usage: small-haskell FILE"
  program <- readFile filePath
  let parsedProgram = parseSmall filePath program
  print parsedProgram
