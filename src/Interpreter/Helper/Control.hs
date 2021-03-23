module Interpreter.Helper.Control where

import Interpreter.Types
import System.Exit
import System.IO

-- | @cond (x, y) b@ returns `x` if `b` is `True` and `y` if `b` is `False`.
cond :: (a, a) -> Bool -> a
cond (x, _) True = x
cond (_, x) False = x

-- | @b ?> (x, y)@ returns `x` if `b` is `True` and `y` if `b` is `False`.
(?>) :: Bool -> (a, a) -> a
b ?> x = cond x b

-- | @putError err@ outputs the error message `err`.
putError :: String -> Ans
putError err = hPutStrLn stderr ("Error: " ++ err) >> return (ExitFailure 1)

-- | `err` is a continuation that always throws an error.
err :: String -> Cc
err err s = putError err >> return (ExitFailure 1)
