module Interpreter.Features.BasicOperations (evalOp) where

import Common.Formatting
import Data.Bits
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Parser.Core.Types
import Text.Printf

-- | @evalOp src o (e1, e2) k s@ calculates the value of operation `o` applied to values `e1` and `e2` with store `s`
-- | then passes the result to the rest of the program `k`.
evalOp :: Exp -> Opr -> (Dv, Dv) -> Ec -> Cc
evalOp _ Mult (DInt a, DInt b) k = k (DInt $ a * b)
evalOp _ Mult (DDouble a, DDouble b) k = k (DDouble $ a * b)
evalOp _ Mult (DInt a, DDouble b) k = k (DDouble $ fromIntegral a * b)
evalOp _ Mult (DDouble a, DInt b) k = k (DDouble $ a * fromIntegral b)
evalOp _ Div (DInt _, DInt 0) k = err "division by 0"
evalOp _ Div (DDouble _, DDouble 0) k = err "division by 0"
evalOp _ Div (DInt _, DDouble 0) k = err "division by 0"
evalOp _ Div (DDouble _, DInt 0) k = err "division by 0"
evalOp _ Div (DInt a, DInt b) k = k (DInt $ a `div` b)
evalOp _ Div (DDouble a, DDouble b) k = k (DDouble $ a / b)
evalOp _ Div (DInt a, DDouble b) k = k (DDouble $ fromIntegral a / b)
evalOp _ Div (DDouble a, DInt b) k = k (DDouble $ a / fromIntegral b)
evalOp _ Mod (DInt _, DInt 0) k = err "modulo by 0"
evalOp _ Mod (DInt a, DInt b) k = k (DInt $ a `mod` b)
evalOp _ Add (DInt a, DInt b) k = k (DInt $ a + b)
evalOp _ Add (DDouble a, DDouble b) k = k (DDouble $ a + b)
evalOp _ Add (DInt a, DDouble b) k = k (DDouble $ fromIntegral a + b)
evalOp _ Add (DDouble a, DInt b) k = k (DDouble $ a + fromIntegral b)
evalOp _ Add (DString a, DString b) k = k (DString $ a ++ b)
evalOp _ Sub (DInt a, DInt b) k = k (DInt $ a - b)
evalOp _ Sub (DDouble a, DDouble b) k = k (DDouble $ a - b)
evalOp _ Sub (DInt a, DDouble b) k = k (DDouble $ fromIntegral a - b)
evalOp _ Sub (DDouble a, DInt b) k = k (DDouble $ a - fromIntegral b)
evalOp _ Less (DInt a, DInt b) k = k (DBool $ a < b)
evalOp _ Less (DDouble a, DDouble b) k = k (DBool $ a < b)
evalOp _ Less (DInt a, DDouble b) k = k (DBool $ fromIntegral a < b)
evalOp _ Less (DDouble a, DInt b) k = k (DBool $ a < fromIntegral b)
evalOp _ LessEq (DInt a, DInt b) k = k (DBool $ a <= b)
evalOp _ LessEq (DDouble a, DDouble b) k = k (DBool $ a <= b)
evalOp _ LessEq (DInt a, DDouble b) k = k (DBool $ fromIntegral a <= b)
evalOp _ LessEq (DDouble a, DInt b) k = k (DBool $ a <= fromIntegral b)
evalOp _ Great (DInt a, DInt b) k = k (DBool $ a > b)
evalOp _ Great (DDouble a, DDouble b) k = k (DBool $ a > b)
evalOp _ Great (DInt a, DDouble b) k = k (DBool $ fromIntegral a > b)
evalOp _ Great (DDouble a, DInt b) k = k (DBool $ a > fromIntegral b)
evalOp _ GreatEq (DInt a, DInt b) k = k (DBool $ a >= b)
evalOp _ GreatEq (DDouble a, DDouble b) k = k (DBool $ a >= b)
evalOp _ GreatEq (DInt a, DDouble b) k = k (DBool $ fromIntegral a >= b)
evalOp _ GreatEq (DDouble a, DInt b) k = k (DBool $ a >= fromIntegral b)
evalOp _ Equal (DInt a, DInt b) k = k (DBool $ a == b)
evalOp _ Equal (DDouble a, DDouble b) k = k (DBool $ a == b)
evalOp _ Equal (DInt a, DDouble b) k = k (DBool $ fromIntegral a == b)
evalOp _ Equal (DDouble a, DInt b) k = k (DBool $ a == fromIntegral b)
evalOp _ Equal (DBool a, DBool b) k = k (DBool $ a == b)
evalOp _ Equal (DString a, DString b) k = k (DBool $ a == b)
evalOp _ NEqual (DInt a, DInt b) k = k (DBool $ a /= b)
evalOp _ NEqual (DDouble a, DDouble b) k = k (DBool $ a /= b)
evalOp _ NEqual (DInt a, DDouble b) k = k (DBool $ fromIntegral a /= b)
evalOp _ NEqual (DDouble a, DInt b) k = k (DBool $ a /= fromIntegral b)
evalOp _ NEqual (DBool a, DBool b) k = k (DBool $ a /= b)
evalOp _ NEqual (DString a, DString b) k = k (DBool $ a /= b)
evalOp _ And (DInt a, DInt b) k = k (DInt $ a .&. b)
evalOp _ And (DBool a, DBool b) k = k (DBool $ a && b)
evalOp _ Xor (DInt a, DInt b) k = k (DInt $ a `xor` b)
evalOp _ Xor (DBool a, DBool b) k = k (DBool $ a `xor` b)
evalOp _ Or (DInt a, DInt b) k = k (DInt $ a .|. b)
evalOp _ Or (DBool a, DBool b) k = k (DBool $ a || b)
evalOp src o (a, b) _ = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in the expression \"%s\"" (pretty o) (typeStr a) (typeStr b) (pretty src)