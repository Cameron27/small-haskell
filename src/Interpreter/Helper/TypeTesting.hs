module Interpreter.Helper.TypeTesting where

-- Type Conversions --

import Common.Formatting
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Types
import Parser.Types
import Text.Printf

-- | @svToDv v` returns the denotable value version of `v`.
svToDv :: Sv -> Dv
svToDv (SInt x) = DInt x
svToDv (SDouble x) = DDouble x
svToDv (SString x) = DString x
svToDv (SBool x) = DBool x
svToDv (SLoc x) = DLoc x
svToDv (SArray x) = DArray x
svToDv (SRecord x) = DRecord x

-- | @svToEv v` returns the expressible value version of `v`.
svToEv :: Sv -> Ev
svToEv = svToDv

-- | @dvToSv d` returns the storable value version of `d`.
dvToSv :: Dv -> Sv
dvToSv (DInt x) = SInt x
dvToSv (DDouble x) = SDouble x
dvToSv (DString x) = SString x
dvToSv (DBool x) = SBool x
dvToSv (DLoc x) = SLoc x
dvToSv (DArray x) = SArray x
dvToSv (DRecord x) = SRecord x
dvToSv e = error $ printf "Tried to convert \"%s\" to a storable value." (pretty e)

-- | @evToSv e` returns the denotable value version of `e`.
evToSv :: Ev -> Sv
evToSv = dvToSv

-- | @dvToRv d` returns the right hand value version of `d`.
dvToRv :: Dv -> Rv
dvToRv (DInt x) = RInt x
dvToRv (DDouble x) = RDouble x
dvToRv (DBool x) = RBool x
dvToRv (DString x) = RString x
dvToRv (DLoc x) = RLoc x
dvToRv (DArray x) = RArray x
dvToRv (DRecord x) = RRecord x
dvToRv e = error $ printf "Tried to convert \"%s\" to a right hand value." (pretty e)

-- | @evToRv e` returns the right hand value version of `e`.
evToRv :: Ev -> Rv
evToRv = dvToRv

-- | @dvToLoc d` returns the location `d` represents.
dvToLoc :: Dv -> Loc
dvToLoc (DLoc x) = x
dvToLoc e = error $ printf "Tried to convert \"%s\" to a location." (pretty e)

-- | @evToLoc e` returns the location `e` represents.
evToLoc :: Ev -> Loc
evToLoc = dvToLoc

-- | @dvToInt d` returns the integer `d` represents.
dvToInt :: Dv -> Integer
dvToInt (DInt x) = x
dvToInt e = error $ printf "Tried to convert \"%s\" to a integer." (pretty e)

-- | @evToInt e` returns the integer `e` represents.
evToInt :: Ev -> Integer
evToInt = dvToInt

-- | @dvToArray d` returns the array `d` represents.
dvToArray :: Dv -> Array
dvToArray (DArray x) = x
dvToArray e = error $ printf "Tried to convert \"%s\" to an array." (pretty e)

-- | @evToArray e` returns the array `e` represents.
evToArray :: Ev -> Array
evToArray = dvToArray

-- | @dvToRecord d` returns the record `d` represents.
dvToRecord :: Dv -> Record
dvToRecord (DRecord x) = x
dvToRecord e = error $ printf "Tried to convert \"%s\" to an record." (pretty e)

-- | @evToRecord e` returns the record `e` represents.
evToRecord :: Ev -> Record
evToRecord = dvToRecord

recordToEnv :: Record -> Env
recordToEnv (Record x) = Env x emptyEc

-- | @dvToFunc d` returns the function `d` represents.
dvToFunc :: Dv -> Function
dvToFunc (DFunc x _) = x
dvToFunc e = error $ printf "Tried to convert \"%s\" to a function." (pretty e)

-- | @evToFunc e` returns the function `e` represents.
evToFunc :: Ev -> Function
evToFunc = dvToFunc

-- | @dvToFunc d` returns the procedure `d` represents.
dvToProc :: Dv -> Procedure
dvToProc (DProc x _) = x
dvToProc e = error $ printf "Tried to convert \"%s\" to a procedure." (pretty e)

-- | @evToFunc e` returns the procedure `e` represents.
evToProc :: Ev -> Procedure
evToProc = dvToProc

-- | @dvToBool d` returns the bool `d` represents.
dvToBool :: Dv -> Bool
dvToBool (DBool x) = x
dvToBool e = error $ printf "Tried to convert \"%s\" to a bool." (pretty e)

-- | @evToBool e` returns the bool `e` represents.
evToBool :: Ev -> Bool
evToBool = dvToBool

-- Type Checks --

-- | @isLoc d@ checks if `d` is a location.
isLoc :: Dv -> Bool
isLoc (DLoc _) = True
isLoc _ = False

-- | @testLoc e1 k e@ applies `e` to `k` if it is a location, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testLoc :: Exp -> Ec -> Ec
testLoc e1 k e = isLoc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a location." (pretty e1) (pretty e))

-- | @isInt d@ checks if `d` is an integer.
isInt :: Dv -> Bool
isInt (DInt _) = True
isInt _ = False

-- | @testInt e1 k e@ applies `e` to `k` if it is a integer, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testInt :: Exp -> Ec -> Ec
testInt e1 k e = isInt e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an integer." (pretty e1) (pretty e))

-- | @isRv e@ checks if `e` is a right hand value.
isRv :: Ev -> Bool
isRv (DInt _) = True
isRv (DDouble _) = True
isRv (DBool _) = True
isRv (DString _) = True
isRv (DLoc _) = True
isRv (DArray _) = True
isRv (DRecord _) = True
isRv _ = False

-- | @testRv e1 k e@ applies `e` to `k` if it is a right hand value, otherwise it returns an error. `e1` is the
-- expression to use in the error message.
testRv :: Exp -> Ec -> Ec
testRv e1 k e = isRv e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a right hand value." (pretty e1) (pretty e))

-- | @isSv e@ checks if `e` is a storable value.
isSv :: Ev -> Bool
isSv (DInt _) = True
isSv (DDouble _) = True
isSv (DBool _) = True
isSv (DString _) = True
isSv (DLoc _) = True
isSv (DArray _) = True
isSv (DRecord _) = True
isSv _ = False

-- | @isFunc e@ checks if `e` is a function.
isFunc :: Ev -> Bool
isFunc (DFunc _ _) = True
isFunc _ = False

-- | @isArray e@ checks if `e` is an array.
isArray :: Ev -> Bool
isArray (DArray _) = True
isArray _ = False

-- | @testArray e1 k e@ applies `e` to `k` if it is an array, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testArray :: Exp -> Ec -> Ec
testArray e1 k e = isArray e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an array." (pretty e1) (pretty e))

-- | @isRecord e@ checks if `e` is a record.
isRecord :: Ev -> Bool
isRecord (DRecord _) = True
isRecord _ = False

-- | @testRecord e1 k e@ applies `e` to `k` if it is a record, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testRecord :: Exp -> Ec -> Ec
testRecord e1 k e = isRecord e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an record." (pretty e1) (pretty e))

-- | @testFunc e1 k e@ applies `e` to `k` if it is a function, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testFunc :: Int -> Exp -> Ec -> Ec
testFunc count e1 k e =
  isFunc e
    ?> ( (n == count)
           ?> ( k e,
                err $ printf "wrong number of arguments supplied to function in \"%s\", expected %d." (pretty e1) n
              ),
         err $ printf "\"%s\", evaluated as \"%s\", is not a function." (pretty e1) (pretty e)
       )
  where
    (DFunc _ n) = e

-- | @isProc e@ checks if `e` is a procedure.
isProc :: Ev -> Bool
isProc (DProc _ _) = True
isProc _ = False

-- | @testProc e1 k e@ applies `e` to `k` if it is a procedure, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testProc :: Int -> Com -> Ec -> Ec
testProc count e1 k e =
  isProc e
    ?> ( (n == count)
           ?> ( k e,
                err $ printf "wrong number of arguments supplied to procedure in \"%s\", expected %d." (pretty e1) n
              ),
         err $ printf "\"%s\", evaluated as \"%s\", is not a procedure." (pretty e1) (pretty e)
       )
  where
    (DProc _ n) = e

-- | @isBool e@ checks if `e` is a bool.
isBool :: Ev -> Bool
isBool (DBool _) = True
isBool _ = False

-- | @testBool e1 k e@ applies `e` to `k` if it is a bool, otherwise it returns an error. `e1` is the expression to use in
-- the error message.
testBool :: Exp -> Ec -> Ec
testBool e1 k e = isBool e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a bool." (pretty e1) (pretty e))

-- | @isCc e@ checks if `e` is a Cc.
isCc :: Ev -> Bool
isCc (DCc _) = True
isCc _ = False

-- | @testCc e1 k e@ applies `e` to `k` if it is a Cc, otherwise it returns an error. `e1` is the expression to use in
-- the error message.
testCc :: Exp -> Ec -> Ec
testCc e1 k e = isCc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a Cc." (pretty e1) (pretty e))