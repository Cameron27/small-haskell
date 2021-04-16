module TypeChecker.BasicOperations (typeOp) where

import Common.Formatting
import qualified Data.Set as Set
import Parser.Types
import Text.Printf
import TypeChecker.Helper.Control
import TypeChecker.Helper.TypeModification
import TypeChecker.Types

typeOp :: Exp -> Opr -> (Type, Type) -> Either TypeError Type
typeOp _ Mult (TInt, TInt) = return TInt
typeOp _ Mult (TDouble, TDouble) = return TDouble
typeOp _ Mult (TInt, TDouble) = return TDouble
typeOp _ Mult (TDouble, TInt) = return TDouble
typeOp _ Div (TInt, TInt) = return TInt
typeOp _ Div (TDouble, TDouble) = return TDouble
typeOp _ Div (TInt, TDouble) = return TDouble
typeOp _ Div (TDouble, TInt) = return TDouble
typeOp _ Mod (TInt, TInt) = return TInt
typeOp _ Add (TInt, TInt) = return TInt
typeOp _ Add (TDouble, TDouble) = return TDouble
typeOp _ Add (TInt, TDouble) = return TDouble
typeOp _ Add (TDouble, TInt) = return TDouble
typeOp _ Add (TString, TString) = return TString
typeOp _ Sub (TInt, TInt) = return TInt
typeOp _ Sub (TDouble, TDouble) = return TDouble
typeOp _ Sub (TInt, TDouble) = return TDouble
typeOp _ Sub (TDouble, TInt) = return TDouble
typeOp _ Less (TInt, TInt) = return TBool
typeOp _ Less (TDouble, TDouble) = return TBool
typeOp _ Less (TInt, TDouble) = return TBool
typeOp _ Less (TDouble, TInt) = return TBool
typeOp _ LessEq (TInt, TInt) = return TBool
typeOp _ LessEq (TDouble, TDouble) = return TBool
typeOp _ LessEq (TInt, TDouble) = return TBool
typeOp _ LessEq (TDouble, TInt) = return TBool
typeOp _ Great (TInt, TInt) = return TBool
typeOp _ Great (TDouble, TDouble) = return TBool
typeOp _ Great (TInt, TDouble) = return TBool
typeOp _ Great (TDouble, TInt) = return TBool
typeOp _ GreatEq (TInt, TInt) = return TBool
typeOp _ GreatEq (TDouble, TDouble) = return TBool
typeOp _ GreatEq (TInt, TDouble) = return TBool
typeOp _ GreatEq (TDouble, TInt) = return TBool
typeOp _ Equal (TInt, TInt) = return TBool
typeOp _ Equal (TDouble, TDouble) = return TBool
typeOp _ Equal (TInt, TDouble) = return TBool
typeOp _ Equal (TDouble, TInt) = return TBool
typeOp _ Equal (TBool, TBool) = return TBool
typeOp _ Equal (TString, TString) = return TBool
typeOp _ NEqual (TInt, TInt) = return TBool
typeOp _ NEqual (TDouble, TDouble) = return TBool
typeOp _ NEqual (TInt, TDouble) = return TBool
typeOp _ NEqual (TDouble, TInt) = return TBool
typeOp _ NEqual (TBool, TBool) = return TBool
typeOp _ NEqual (TString, TString) = return TBool
typeOp _ And (TInt, TInt) = return TInt
typeOp _ And (TBool, TBool) = return TBool
typeOp _ Xor (TInt, TInt) = return TInt
typeOp _ Xor (TBool, TBool) = return TBool
typeOp _ Or (TInt, TInt) = return TInt
typeOp _ Or (TBool, TBool) = return TBool
typeOp e o (a, b) = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in \"%s\"" (pretty o) (show a) (show b) (pretty e)