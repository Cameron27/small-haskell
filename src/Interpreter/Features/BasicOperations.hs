module Interpreter.Features.BasicOperations (evalOp) where

import Common.Formatting
import Data.Bits
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Parser.Core.Types
import Text.Printf

-- | @evalOp src o (e1, e2) k s@ calculates the value of operation `o` applied to values `e1` and `e2` with store `s`
-- then passes the result to the rest of the program `k`.
evalOp :: Exp -> Opr -> (Ev, Ev) -> Ec -> Cc
evalOp _ Mult (EInt a, EInt b) k = k (EInt $ a * b)
evalOp _ Mult (EDouble a, EDouble b) k = k (EDouble $ a * b)
evalOp _ Mult (EInt a, EDouble b) k = k (EDouble $ fromIntegral a * b)
evalOp _ Mult (EDouble a, EInt b) k = k (EDouble $ a * fromIntegral b)
evalOp _ Div (EInt _, EInt 0) k = err "division by 0"
evalOp _ Div (EDouble _, EDouble 0) k = err "division by 0"
evalOp _ Div (EInt _, EDouble 0) k = err "division by 0"
evalOp _ Div (EDouble _, EInt 0) k = err "division by 0"
evalOp _ Div (EInt a, EInt b) k = k (EInt $ a `div` b)
evalOp _ Div (EDouble a, EDouble b) k = k (EDouble $ a / b)
evalOp _ Div (EInt a, EDouble b) k = k (EDouble $ fromIntegral a / b)
evalOp _ Div (EDouble a, EInt b) k = k (EDouble $ a / fromIntegral b)
evalOp _ Mod (EInt _, EInt 0) k = err "modulo by 0"
evalOp _ Mod (EInt a, EInt b) k = k (EInt $ a `mod` b)
evalOp _ Add (EInt a, EInt b) k = k (EInt $ a + b)
evalOp _ Add (EDouble a, EDouble b) k = k (EDouble $ a + b)
evalOp _ Add (EInt a, EDouble b) k = k (EDouble $ fromIntegral a + b)
evalOp _ Add (EDouble a, EInt b) k = k (EDouble $ a + fromIntegral b)
evalOp _ Add (EString a, EString b) k = k (EString $ a ++ b)
evalOp _ Sub (EInt a, EInt b) k = k (EInt $ a - b)
evalOp _ Sub (EDouble a, EDouble b) k = k (EDouble $ a - b)
evalOp _ Sub (EInt a, EDouble b) k = k (EDouble $ fromIntegral a - b)
evalOp _ Sub (EDouble a, EInt b) k = k (EDouble $ a - fromIntegral b)
evalOp _ Less (EInt a, EInt b) k = k (EBool $ a < b)
evalOp _ Less (EDouble a, EDouble b) k = k (EBool $ a < b)
evalOp _ Less (EInt a, EDouble b) k = k (EBool $ fromIntegral a < b)
evalOp _ Less (EDouble a, EInt b) k = k (EBool $ a < fromIntegral b)
evalOp _ LessEq (EInt a, EInt b) k = k (EBool $ a <= b)
evalOp _ LessEq (EDouble a, EDouble b) k = k (EBool $ a <= b)
evalOp _ LessEq (EInt a, EDouble b) k = k (EBool $ fromIntegral a <= b)
evalOp _ LessEq (EDouble a, EInt b) k = k (EBool $ a <= fromIntegral b)
evalOp _ Great (EInt a, EInt b) k = k (EBool $ a > b)
evalOp _ Great (EDouble a, EDouble b) k = k (EBool $ a > b)
evalOp _ Great (EInt a, EDouble b) k = k (EBool $ fromIntegral a > b)
evalOp _ Great (EDouble a, EInt b) k = k (EBool $ a > fromIntegral b)
evalOp _ GreatEq (EInt a, EInt b) k = k (EBool $ a >= b)
evalOp _ GreatEq (EDouble a, EDouble b) k = k (EBool $ a >= b)
evalOp _ GreatEq (EInt a, EDouble b) k = k (EBool $ fromIntegral a >= b)
evalOp _ GreatEq (EDouble a, EInt b) k = k (EBool $ a >= fromIntegral b)
evalOp _ Equal (EInt a, EInt b) k = k (EBool $ a == b)
evalOp _ Equal (EDouble a, EDouble b) k = k (EBool $ a == b)
evalOp _ Equal (EInt a, EDouble b) k = k (EBool $ fromIntegral a == b)
evalOp _ Equal (EDouble a, EInt b) k = k (EBool $ a == fromIntegral b)
evalOp _ Equal (EBool a, EBool b) k = k (EBool $ a == b)
evalOp _ Equal (EString a, EString b) k = k (EBool $ a == b)
evalOp _ NEqual (EInt a, EInt b) k = k (EBool $ a /= b)
evalOp _ NEqual (EDouble a, EDouble b) k = k (EBool $ a /= b)
evalOp _ NEqual (EInt a, EDouble b) k = k (EBool $ fromIntegral a /= b)
evalOp _ NEqual (EDouble a, EInt b) k = k (EBool $ a /= fromIntegral b)
evalOp _ NEqual (EBool a, EBool b) k = k (EBool $ a /= b)
evalOp _ NEqual (EString a, EString b) k = k (EBool $ a /= b)
evalOp _ And (EInt a, EInt b) k = k (EInt $ a .&. b)
evalOp _ And (EBool a, EBool b) k = k (EBool $ a && b)
evalOp _ Xor (EInt a, EInt b) k = k (EInt $ a `xor` b)
evalOp _ Xor (EBool a, EBool b) k = k (EBool $ a `xor` b)
evalOp _ Or (EInt a, EInt b) k = k (EInt $ a .|. b)
evalOp _ Or (EBool a, EBool b) k = k (EBool $ a || b)
evalOp src o (a, b) _ = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in the expression \"%s\"" (pretty o) (typeStr a) (typeStr b) (pretty src)