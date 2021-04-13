module Main where

import Control.Monad
import Interpreter.Small
import Interpreter.Types
import Parser.Small
import System.Console.ParseArgs
import System.Environment
import System.Exit

data Arguments = PrintParsed | IgnoreTypes | FileName
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  args <-
    parseArgsIO
      ArgsComplete
      [ Arg
          { argIndex = PrintParsed,
            argAbbr = Just 'p',
            argName = Just "print-parsed",
            argData = Nothing,
            argDesc = "Print the parsed output"
          },
        Arg
          { argIndex = IgnoreTypes,
            argAbbr = Just 't',
            argName = Just "ignore-types",
            argData = Nothing,
            argDesc = "Do not type check the program"
          },
        Arg
          { argIndex = FileName,
            argAbbr = Nothing,
            argName = Nothing,
            argData = argDataRequired "FILE" ArgtypeString,
            argDesc = "Do not type check the program"
          }
      ]
  let printParsed = gotArg args PrintParsed
  let ignoreTypes = gotArg args IgnoreTypes
  let Just fileName = getArgString args FileName

  program <- readFile fileName
  let parsedProgram' = parseSmall fileName program
  parsedProgram <- case parsedProgram' of
    Right p -> return p
    Left err -> die $show err
  when printParsed $ print parsedProgram
  exitCode <- interpretSmall parsedProgram
  exitWith exitCode
