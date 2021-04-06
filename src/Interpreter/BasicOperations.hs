module Interpreter.BasicOperations (evalOp) where

import Common.Formatting
import Data.Bits
import Interpreter.Helper.Control
import Interpreter.Types
import Parser.Types
import Text.Printf

evalOp :: Exp -> Opr -> (Rv, Rv) -> Ec -> Cc
evalOp _ Mult (RInt a, RInt b) k = k (DInt $ a * b)
evalOp _ Mult (RDouble a, RDouble b) k = k (DDouble $ a * b)
evalOp _ Mult (RInt a, RDouble b) k = k (DDouble $ fromIntegral a * b)
evalOp _ Mult (RDouble a, RInt b) k = k (DDouble $ a * fromIntegral b)
evalOp _ Div (RInt _, RInt 0) k = err "division by 0"
evalOp _ Div (RDouble _, RDouble 0) k = err "division by 0"
evalOp _ Div (RInt _, RDouble 0) k = err "division by 0"
evalOp _ Div (RDouble _, RInt 0) k = err "division by 0"
evalOp _ Div (RInt a, RInt b) k = k (DInt $ a `div` b)
evalOp _ Div (RDouble a, RDouble b) k = k (DDouble $ a / b)
evalOp _ Div (RInt a, RDouble b) k = k (DDouble $ fromIntegral a / b)
evalOp _ Div (RDouble a, RInt b) k = k (DDouble $ a / fromIntegral b)
evalOp _ Mod (RInt _, RInt 0) k = err "modulo by 0"
evalOp _ Mod (RInt a, RInt b) k = k (DInt $ a `mod` b)
evalOp _ Add (RInt a, RInt b) k = k (DInt $ a + b)
evalOp _ Add (RDouble a, RDouble b) k = k (DDouble $ a + b)
evalOp _ Add (RInt a, RDouble b) k = k (DDouble $ fromIntegral a + b)
evalOp _ Add (RDouble a, RInt b) k = k (DDouble $ a + fromIntegral b)
evalOp _ Add (RString a, RString b) k = k (DString $ a ++ b)
evalOp _ Sub (RInt a, RInt b) k = k (DInt $ a - b)
evalOp _ Sub (RDouble a, RDouble b) k = k (DDouble $ a - b)
evalOp _ Sub (RInt a, RDouble b) k = k (DDouble $ fromIntegral a - b)
evalOp _ Sub (RDouble a, RInt b) k = k (DDouble $ a - fromIntegral b)
evalOp _ Less (RInt a, RInt b) k = k (DBool $ a < b)
evalOp _ Less (RDouble a, RDouble b) k = k (DBool $ a < b)
evalOp _ Less (RInt a, RDouble b) k = k (DBool $ fromIntegral a < b)
evalOp _ Less (RDouble a, RInt b) k = k (DBool $ a < fromIntegral b)
evalOp _ LessEq (RInt a, RInt b) k = k (DBool $ a <= b)
evalOp _ LessEq (RDouble a, RDouble b) k = k (DBool $ a <= b)
evalOp _ LessEq (RInt a, RDouble b) k = k (DBool $ fromIntegral a <= b)
evalOp _ LessEq (RDouble a, RInt b) k = k (DBool $ a <= fromIntegral b)
evalOp _ Great (RInt a, RInt b) k = k (DBool $ a > b)
evalOp _ Great (RDouble a, RDouble b) k = k (DBool $ a > b)
evalOp _ Great (RInt a, RDouble b) k = k (DBool $ fromIntegral a > b)
evalOp _ Great (RDouble a, RInt b) k = k (DBool $ a > fromIntegral b)
evalOp _ GreatEq (RInt a, RInt b) k = k (DBool $ a >= b)
evalOp _ GreatEq (RDouble a, RDouble b) k = k (DBool $ a >= b)
evalOp _ GreatEq (RInt a, RDouble b) k = k (DBool $ fromIntegral a >= b)
evalOp _ GreatEq (RDouble a, RInt b) k = k (DBool $ a >= fromIntegral b)
evalOp _ Equal (RInt a, RInt b) k = k (DBool $ a == b)
evalOp _ Equal (RDouble a, RDouble b) k = k (DBool $ a == b)
evalOp _ Equal (RInt a, RDouble b) k = k (DBool $ fromIntegral a == b)
evalOp _ Equal (RDouble a, RInt b) k = k (DBool $ a == fromIntegral b)
evalOp _ Equal (RBool a, RBool b) k = k (DBool $ a == b)
evalOp _ Equal (RString a, RString b) k = k (DBool $ a == b)
evalOp _ NEqual (RInt a, RInt b) k = k (DBool $ a /= b)
evalOp _ NEqual (RDouble a, RDouble b) k = k (DBool $ a /= b)
evalOp _ NEqual (RInt a, RDouble b) k = k (DBool $ fromIntegral a /= b)
evalOp _ NEqual (RDouble a, RInt b) k = k (DBool $ a /= fromIntegral b)
evalOp _ NEqual (RBool a, RBool b) k = k (DBool $ a /= b)
evalOp _ NEqual (RString a, RString b) k = k (DBool $ a /= b)
evalOp _ And (RInt a, RInt b) k = k (DInt $ a .&. b)
evalOp _ And (RBool a, RBool b) k = k (DBool $ a && b)
evalOp _ Xor (RInt a, RInt b) k = k (DInt $ a `xor` b)
evalOp _ Xor (RBool a, RBool b) k = k (DBool $ a `xor` b)
evalOp _ Or (RInt a, RInt b) k = k (DInt $ a .|. b)
evalOp _ Or (RBool a, RBool b) k = k (DBool $ a || b)
evalOp e1 o (a, b) _ = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in the expression \"%s\"" (pretty o) (typeStr a) (typeStr b) (pretty e1)