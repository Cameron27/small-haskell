module Interpreter.Features.BasicOperations (evalOp2, evalOp1) where

import Common.Formatting
import Data.Bits
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Parser.Core.Types
import Text.Printf

-- | @evalOp2 src o (e1, e2) k s@ calculates the value of operation `o` applied to values `e1` and `e2` with store `s`
-- then passes the result to the rest of the program `k`.
evalOp2 :: Exp -> Opr2 -> (Ev, Ev) -> Ec -> Cc
evalOp2 _ Mult (EInt a, EInt b) k = k (EInt $ a * b)
evalOp2 _ Mult (EDouble a, EDouble b) k = k (EDouble $ a * b)
evalOp2 _ Mult (EInt a, EDouble b) k = k (EDouble $ fromIntegral a * b)
evalOp2 _ Mult (EDouble a, EInt b) k = k (EDouble $ a * fromIntegral b)
evalOp2 _ Div (EInt _, EInt 0) k = err "division by 0"
evalOp2 _ Div (EDouble _, EDouble 0) k = err "division by 0"
evalOp2 _ Div (EInt _, EDouble 0) k = err "division by 0"
evalOp2 _ Div (EDouble _, EInt 0) k = err "division by 0"
evalOp2 _ Div (EInt a, EInt b) k = k (EInt $ a `div` b)
evalOp2 _ Div (EDouble a, EDouble b) k = k (EDouble $ a / b)
evalOp2 _ Div (EInt a, EDouble b) k = k (EDouble $ fromIntegral a / b)
evalOp2 _ Div (EDouble a, EInt b) k = k (EDouble $ a / fromIntegral b)
evalOp2 _ Mod (EInt _, EInt 0) k = err "modulo by 0"
evalOp2 _ Mod (EInt a, EInt b) k = k (EInt $ a `mod` b)
evalOp2 _ Add (EInt a, EInt b) k = k (EInt $ a + b)
evalOp2 _ Add (EDouble a, EDouble b) k = k (EDouble $ a + b)
evalOp2 _ Add (EInt a, EDouble b) k = k (EDouble $ fromIntegral a + b)
evalOp2 _ Add (EDouble a, EInt b) k = k (EDouble $ a + fromIntegral b)
evalOp2 _ Add (EString a, EString b) k = k (EString $ a ++ b)
evalOp2 _ Sub (EInt a, EInt b) k = k (EInt $ a - b)
evalOp2 _ Sub (EDouble a, EDouble b) k = k (EDouble $ a - b)
evalOp2 _ Sub (EInt a, EDouble b) k = k (EDouble $ fromIntegral a - b)
evalOp2 _ Sub (EDouble a, EInt b) k = k (EDouble $ a - fromIntegral b)
evalOp2 _ Less (EInt a, EInt b) k = k (EBool $ a < b)
evalOp2 _ Less (EDouble a, EDouble b) k = k (EBool $ a < b)
evalOp2 _ Less (EInt a, EDouble b) k = k (EBool $ fromIntegral a < b)
evalOp2 _ Less (EDouble a, EInt b) k = k (EBool $ a < fromIntegral b)
evalOp2 _ LessEq (EInt a, EInt b) k = k (EBool $ a <= b)
evalOp2 _ LessEq (EDouble a, EDouble b) k = k (EBool $ a <= b)
evalOp2 _ LessEq (EInt a, EDouble b) k = k (EBool $ fromIntegral a <= b)
evalOp2 _ LessEq (EDouble a, EInt b) k = k (EBool $ a <= fromIntegral b)
evalOp2 _ Great (EInt a, EInt b) k = k (EBool $ a > b)
evalOp2 _ Great (EDouble a, EDouble b) k = k (EBool $ a > b)
evalOp2 _ Great (EInt a, EDouble b) k = k (EBool $ fromIntegral a > b)
evalOp2 _ Great (EDouble a, EInt b) k = k (EBool $ a > fromIntegral b)
evalOp2 _ GreatEq (EInt a, EInt b) k = k (EBool $ a >= b)
evalOp2 _ GreatEq (EDouble a, EDouble b) k = k (EBool $ a >= b)
evalOp2 _ GreatEq (EInt a, EDouble b) k = k (EBool $ fromIntegral a >= b)
evalOp2 _ GreatEq (EDouble a, EInt b) k = k (EBool $ a >= fromIntegral b)
evalOp2 _ Equal (EInt a, EInt b) k = k (EBool $ a == b)
evalOp2 _ Equal (EDouble a, EDouble b) k = k (EBool $ a == b)
evalOp2 _ Equal (EInt a, EDouble b) k = k (EBool $ fromIntegral a == b)
evalOp2 _ Equal (EDouble a, EInt b) k = k (EBool $ a == fromIntegral b)
evalOp2 _ Equal (EBool a, EBool b) k = k (EBool $ a == b)
evalOp2 _ Equal (EString a, EString b) k = k (EBool $ a == b)
evalOp2 _ NEqual (EInt a, EInt b) k = k (EBool $ a /= b)
evalOp2 _ NEqual (EDouble a, EDouble b) k = k (EBool $ a /= b)
evalOp2 _ NEqual (EInt a, EDouble b) k = k (EBool $ fromIntegral a /= b)
evalOp2 _ NEqual (EDouble a, EInt b) k = k (EBool $ a /= fromIntegral b)
evalOp2 _ NEqual (EBool a, EBool b) k = k (EBool $ a /= b)
evalOp2 _ NEqual (EString a, EString b) k = k (EBool $ a /= b)
evalOp2 _ And (EInt a, EInt b) k = k (EInt $ a .&. b)
evalOp2 _ And (EBool a, EBool b) k = k (EBool $ a && b)
evalOp2 _ Xor (EInt a, EInt b) k = k (EInt $ a `xor` b)
evalOp2 _ Xor (EBool a, EBool b) k = k (EBool $ a `xor` b)
evalOp2 _ Or (EInt a, EInt b) k = k (EInt $ a .|. b)
evalOp2 _ Or (EBool a, EBool b) k = k (EBool $ a || b)
evalOp2 src o (a, b) _ = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in the expression \"%s\"" (pretty o) (typeStr a) (typeStr b) (pretty src)

-- | @evalOp1 src o e k s@ calculates the value of operation `o` applied to values `e` with store `s` then passes the
-- result to the rest of the program `k`.
evalOp1 :: Exp -> Opr1 -> Ev -> Ec -> Cc
evalOp1 _ Not (EBool a) k = k (EBool $ not a)
evalOp1 _ Positive (EInt a) k = k (EInt a)
evalOp1 _ Positive (EDouble a) k = k (EDouble a)
evalOp1 _ Negative (EInt a) k = k (EInt $ - a)
evalOp1 _ Negative (EDouble a) k = k (EDouble $ - a)
evalOp1 src o a _ = err $ printf "operation \"%s\" cannot be applied to type \"%s\" in the expression \"%s\"" (pretty o) (typeStr a) (pretty src)