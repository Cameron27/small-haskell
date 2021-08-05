module TypeChecker.Features.BasicOperations (typeOp2, typeOp1) where

import Common.Formatting
import Parser.Core.Types
import Text.Printf
import TypeChecker.Core.Types
import TypeChecker.Helper.Control

-- | @typeOp2 src o (t1, t2)@ returns the type obtained from applying operation `o` to types `t1` and `t2`. `src` is the
-- expression to use in the error message.
typeOp2 :: Exp -> Opr2 -> (Type, Type) -> TypeResult Type
typeOp2 _ Mult (TInt, TInt) = return TInt
typeOp2 _ Mult (TDouble, TDouble) = return TDouble
typeOp2 _ Mult (TInt, TDouble) = return TDouble
typeOp2 _ Mult (TDouble, TInt) = return TDouble
typeOp2 _ Div (TInt, TInt) = return TInt
typeOp2 _ Div (TDouble, TDouble) = return TDouble
typeOp2 _ Div (TInt, TDouble) = return TDouble
typeOp2 _ Div (TDouble, TInt) = return TDouble
typeOp2 _ Mod (TInt, TInt) = return TInt
typeOp2 _ Add (TInt, TInt) = return TInt
typeOp2 _ Add (TDouble, TDouble) = return TDouble
typeOp2 _ Add (TInt, TDouble) = return TDouble
typeOp2 _ Add (TDouble, TInt) = return TDouble
typeOp2 _ Add (TString, TString) = return TString
typeOp2 _ Sub (TInt, TInt) = return TInt
typeOp2 _ Sub (TDouble, TDouble) = return TDouble
typeOp2 _ Sub (TInt, TDouble) = return TDouble
typeOp2 _ Sub (TDouble, TInt) = return TDouble
typeOp2 _ Less (TInt, TInt) = return TBool
typeOp2 _ Less (TDouble, TDouble) = return TBool
typeOp2 _ Less (TInt, TDouble) = return TBool
typeOp2 _ Less (TDouble, TInt) = return TBool
typeOp2 _ LessEq (TInt, TInt) = return TBool
typeOp2 _ LessEq (TDouble, TDouble) = return TBool
typeOp2 _ LessEq (TInt, TDouble) = return TBool
typeOp2 _ LessEq (TDouble, TInt) = return TBool
typeOp2 _ Great (TInt, TInt) = return TBool
typeOp2 _ Great (TDouble, TDouble) = return TBool
typeOp2 _ Great (TInt, TDouble) = return TBool
typeOp2 _ Great (TDouble, TInt) = return TBool
typeOp2 _ GreatEq (TInt, TInt) = return TBool
typeOp2 _ GreatEq (TDouble, TDouble) = return TBool
typeOp2 _ GreatEq (TInt, TDouble) = return TBool
typeOp2 _ GreatEq (TDouble, TInt) = return TBool
typeOp2 _ Equal (TInt, TInt) = return TBool
typeOp2 _ Equal (TDouble, TDouble) = return TBool
typeOp2 _ Equal (TInt, TDouble) = return TBool
typeOp2 _ Equal (TDouble, TInt) = return TBool
typeOp2 _ Equal (TBool, TBool) = return TBool
typeOp2 _ Equal (TString, TString) = return TBool
typeOp2 _ NEqual (TInt, TInt) = return TBool
typeOp2 _ NEqual (TDouble, TDouble) = return TBool
typeOp2 _ NEqual (TInt, TDouble) = return TBool
typeOp2 _ NEqual (TDouble, TInt) = return TBool
typeOp2 _ NEqual (TBool, TBool) = return TBool
typeOp2 _ NEqual (TString, TString) = return TBool
typeOp2 _ And (TInt, TInt) = return TInt
typeOp2 _ And (TBool, TBool) = return TBool
typeOp2 _ Xor (TInt, TInt) = return TInt
typeOp2 _ Xor (TBool, TBool) = return TBool
typeOp2 _ Or (TInt, TInt) = return TInt
typeOp2 _ Or (TBool, TBool) = return TBool
typeOp2 e o (a, b) = err $ printf "operation \"%s\" cannot be applied to types \"%s\" and \"%s\" in \"%s\"." (pretty o) (show a) (show b) (pretty e)

-- | @typeOp1 src o t1@ returns the type obtained from applying operation `o` to types `t1` . `src` is the expression to
-- use in the error message.
typeOp1 :: Exp -> Opr1 -> Type -> TypeResult Type
typeOp1 _ Not TBool = return TBool
typeOp1 _ Positive TInt = return TInt
typeOp1 _ Positive TDouble = return TDouble
typeOp1 _ Negative TInt = return TInt
typeOp1 _ Negative TDouble = return TDouble
typeOp1 e o a = err $ printf "operation \"%s\" cannot be applied to type \"%s\" in \"%s\"." (pretty o) (show a) (pretty e)
