module Interpreter.Helper.TypeTesting where

-- Type Conversions --

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Parser.Core.Types
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
svToDv (SFile x) = DFile x
svToDv (SObject x) = DObject x
svToDv SNull = DNull
svToDv v = error $ printf "Tried to convert \"%s\" to a denotable value." (pretty v)

-- | @svToEv v` returns the expressible value version of `v`.
svToEv :: Sv -> Ev
svToEv = svToDv

-- | @svToRv v` returns the denotable value version of `v`.
svToRv :: Sv -> Rv
svToRv (SInt x) = RInt x
svToRv (SDouble x) = RDouble x
svToRv (SString x) = RString x
svToRv (SBool x) = RBool x
svToRv (SLoc x) = RLoc x
svToRv (SArray x) = RArray x
svToRv (SRecord x) = RRecord x
svToRv (SObject x) = RObject x
svToRv SNull = RNull
svToRv v = error $ printf "Tried to convert \"%s\" to a denotable value." (pretty v)

-- | @dvToSv d` returns the storable value version of `d`.
dvToSv :: Dv -> Sv
dvToSv (DInt x) = SInt x
dvToSv (DDouble x) = SDouble x
dvToSv (DString x) = SString x
dvToSv (DBool x) = SBool x
dvToSv (DLoc x) = SLoc x
dvToSv (DArray x) = SArray x
dvToSv (DRecord x) = SRecord x
dvToSv (DFile x) = SFile x
dvToSv (DObject x) = SObject x
dvToSv DNull = SNull
dvToSv e = error $ printf "Tried to convert \"%s\" to a storable value." (pretty e)

-- | @evToSv e@ returns the denotable value version of `e`.
evToSv :: Ev -> Sv
evToSv = dvToSv

-- | @dvToRv d@ returns the right hand value version of `d`.
dvToRv :: Dv -> Rv
dvToRv (DInt x) = RInt x
dvToRv (DDouble x) = RDouble x
dvToRv (DBool x) = RBool x
dvToRv (DString x) = RString x
dvToRv (DLoc x) = RLoc x
dvToRv (DArray x) = RArray x
dvToRv (DRecord x) = RRecord x
dvToRv (DObject x) = RObject x
dvToRv DNull = RNull
dvToRv e = error $ printf "Tried to convert \"%s\" to a right hand value." (pretty e)

-- | @evToRv e@ returns the right hand value version of `e`.
evToRv :: Ev -> Rv
evToRv = dvToRv

-- | @rvToSv e@ returns the storable version of `e`.
rvToSv :: Rv -> Sv
rvToSv (RInt x) = SInt x
rvToSv (RDouble x) = SDouble x
rvToSv (RBool x) = SBool x
rvToSv (RString x) = SString x
rvToSv (RLoc x) = SLoc x
rvToSv (RArray x) = SArray x
rvToSv (RRecord x) = SRecord x
rvToSv (RObject x) = SObject x
rvToSv RNull = SNull
rvToSv e = error $ printf "Tried to convert \"%s\" to a right hand value." "CANNOT PRETTY"

-- | @dvToLoc d@ returns the location `d` represents.
dvToLoc :: Dv -> Loc
dvToLoc (DLoc x) = x
dvToLoc e = error $ printf "Tried to convert \"%s\" to a location." (pretty e)

-- | @evToLoc e@ returns the location `e` represents.
evToLoc :: Ev -> Loc
evToLoc = dvToLoc

-- | @dvToInt d@ returns the integer `d` represents.
dvToInt :: Dv -> Int
dvToInt (DInt x) = x
dvToInt e = error $ printf "Tried to convert \"%s\" to a integer." (pretty e)

-- | @evToInt e@ returns the integer `e` represents.
evToInt :: Ev -> Int
evToInt = dvToInt

-- | @dvToDouble d@ returns the double `d` represents.
dvToDouble :: Dv -> Double
dvToDouble (DDouble x) = x
dvToDouble e = error $ printf "Tried to convert \"%s\" to a double." (pretty e)

-- | @evToDouble e@ returns the double `e` represents.
evToDouble :: Ev -> Double
evToDouble = dvToDouble

-- | @dvToArray d@ returns the array `d` represents.
dvToArray :: Dv -> Array
dvToArray (DArray x) = x
dvToArray e = error $ printf "Tried to convert \"%s\" to an array." (pretty e)

-- | @evToArray e@ returns the array `e` represents.
evToArray :: Ev -> Array
evToArray = dvToArray

-- | @dvToRecord d@ returns the record `d` represents.
dvToRecord :: Dv -> Record
dvToRecord (DRecord x) = x
dvToRecord e = error $ printf "Tried to convert \"%s\" to an record." (pretty e)

-- | @evToRecord e@ returns the record `e` represents.
evToRecord :: Ev -> Record
evToRecord = dvToRecord

-- | @dvToFunc d@ returns the function `d` represents.
dvToFunc :: Dv -> Function
dvToFunc (DFunc x _) = x
dvToFunc e = error $ printf "Tried to convert \"%s\" to a function." (pretty e)

-- | @evToFunc e@ returns the function `e` represents.
evToFunc :: Ev -> Function
evToFunc = dvToFunc

-- | @dvToFunc d@ returns the procedure `d` represents.
dvToProc :: Dv -> Procedure
dvToProc (DProc x _) = x
dvToProc e = error $ printf "Tried to convert \"%s\" to a procedure." (pretty e)

-- | @evToFunc e@ returns the procedure `e` represents.
evToProc :: Ev -> Procedure
evToProc = dvToProc

-- | @dvToClass d@ returns the class `d` represents.
dvToClass :: Dv -> Class
dvToClass (DClass x) = x
dvToClass e = error $ printf "Tried to convert \"%s\" to a class." (pretty e)

-- | @evToClass e@ returns the class `e` represents.
evToClass :: Ev -> Class
evToClass = dvToClass

-- | @dvToObject d@ returns the object `d` represents.
dvToObject :: Dv -> Object
dvToObject (DObject x) = x
dvToObject e = error $ printf "Tried to convert \"%s\" to an object." (pretty e)

-- | @evToObject e@ returns the object `e` represents.
evToObject :: Ev -> Object
evToObject = dvToObject

-- | @dvToBool d@ returns the bool `d` represents.
dvToBool :: Dv -> Bool
dvToBool (DBool x) = x
dvToBool e = error $ printf "Tried to convert \"%s\" to a bool." (pretty e)

-- | @evToBool e@ returns the bool `e` represents.
evToBool :: Ev -> Bool
evToBool = dvToBool

-- | @recordToEnv r@ returns the environment `r` represents.
recordToEnv :: Record -> Env
recordToEnv (Record r) = Env r HashMap.empty emptyEc emptyObj

-- | @objectToEnv r@ returns the environment `r` represents.
objectToEnv :: Object -> Env
objectToEnv (Object r) = Env r HashMap.empty emptyEc emptyObj

-- Type Checks --

-- | @isLoc d@ checks if `d` is a location.
isLoc :: Dv -> Bool
isLoc (DLoc _) = True
isLoc _ = False

-- | @testLoc src k e@ applies `e` to `k` if it is a location, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testLoc :: Exp -> Ec -> Ec
testLoc src k e = isLoc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a location." (pretty src) (pretty e))

-- | @isInt d@ checks if `d` is an integer.
isInt :: Dv -> Bool
isInt (DInt _) = True
isInt _ = False

-- | @testInt src k e@ applies `e` to `k` if it is a integer, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testInt :: Exp -> Ec -> Ec
testInt src k e = isInt e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an integer." (pretty src) (pretty e))

-- | @isNumber d@ checks if `d` is a number.
isNumber :: Dv -> Bool
isNumber (DInt _) = True
isNumber (DDouble _) = True
isNumber _ = False

-- | @testNumber src k e@ applies `e` to `k` if it is a number, otherwise it returns an error. `src` is the expression
-- to use in the error message.
testNumber :: Exp -> Ec -> Ec
testNumber src k e = isNumber e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a number." (pretty src) (pretty e))

-- | @isRv e@ checks if `e` is a right hand value.
isRv :: Ev -> Bool
isRv (DInt _) = True
isRv (DDouble _) = True
isRv (DBool _) = True
isRv (DString _) = True
isRv (DLoc _) = True
isRv (DArray _) = True
isRv (DRecord _) = True
isRv (DObject _) = True
isRv DNull = True
isRv _ = False

-- | @testRv src k e@ applies `e` to `k` if it is a right hand value, otherwise it returns an error. `src` is the
-- expression to use in the error message.
testRv :: Exp -> Ec -> Ec
testRv src k e = isRv e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a right hand value." (pretty src) (pretty e))

-- | @isSv e@ checks if `e` is a storable value.
isSv :: Ev -> Bool
isSv (DInt _) = True
isSv (DDouble _) = True
isSv (DBool _) = True
isSv (DString _) = True
isSv (DLoc _) = True
isSv (DArray _) = True
isSv (DRecord _) = True
isSv (DObject _) = True
isSv DNull = True
isSv _ = False

-- | @isProc e@ checks if `e` is a procedure.
isProc :: Ev -> Bool
isProc (DProc _ _) = True
isProc _ = False

-- | @testProc src k e@ applies `e` to `k` if it is a procedure, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testProc :: Int -> Com -> Ec -> Ec
testProc count src k e =
  isProc e
    ?> ( (n == count)
           ?> ( k e,
                err $ printf "wrong number of arguments supplied to procedure in \"%s\", expected %d." (pretty src) n
              ),
         err $ printf "\"%s\", evaluated as \"%s\", is not a procedure." (pretty src) (pretty e)
       )
  where
    (DProc _ n) = e

-- | @isMethod e@ checks if `e` is a method.
isMethod :: Ev -> Bool
isMethod (DMethod _) = True
isMethod _ = False

-- | @testMethod src k e@ applies `e` to `k` if it is a method, otherwise it returns an error. `src` is the
-- expression to use in the error message.
testMethod :: Exp -> Ec -> Ec
testMethod src k e = isMethod e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a method." (pretty src) (pretty e))

-- | @isFunc e@ checks if `e` is a function.
isFunc :: Ev -> Bool
isFunc (DFunc _ _) = True
isFunc _ = False

-- | @testFunc src k e@ applies `e` to `k` if it is a function, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testFunc :: Int -> Exp -> Ec -> Ec
testFunc count src k e =
  isFunc e
    ?> ( (n == count)
           ?> ( k e,
                err $ printf "wrong number of arguments supplied to function in \"%s\", expected %d." (pretty src) n
              ),
         err $ printf "\"%s\", evaluated as \"%s\", is not a function." (pretty src) (pretty e)
       )
  where
    (DFunc _ n) = e

-- | @isClass e@ checks if `e` is a class.
isClass :: Ev -> Bool
isClass (DClass _) = True
isClass _ = False

-- | @testClass src k e@ applies `e` to `k` if it is a class, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testClass :: Exp -> Ec -> Ec
testClass src k e = isClass e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a class." (pretty src) (pretty e))

-- | @isObject e@ checks if `e` is an object.
isObject :: Ev -> Bool
isObject (DObject _) = True
isObject _ = False

-- | @testObject src k e@ applies `e` to `k` if it is an object, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testObject :: Exp -> Ec -> Ec
testObject src k e = isObject e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an object." (pretty src) (pretty e))

-- | @isArray e@ checks if `e` is an array.
isArray :: Ev -> Bool
isArray (DArray _) = True
isArray _ = False

-- | @testArray src k e@ applies `e` to `k` if it is an array, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testArray :: Exp -> Ec -> Ec
testArray src k e = isArray e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an array." (pretty src) (pretty e))

-- | @isRecord e@ checks if `e` is a record.
isRecord :: Ev -> Bool
isRecord (DRecord _) = True
isRecord _ = False

-- | @testRecord src k e@ applies `e` to `k` if it is a record, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testRecord :: Exp -> Ec -> Ec
testRecord src k e = isRecord e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a record." (pretty src) (pretty e))

-- | @isBool e@ checks if `e` is a bool.
isBool :: Ev -> Bool
isBool (DBool _) = True
isBool _ = False

-- | @testBool src k e@ applies `e` to `k` if it is a bool, otherwise it returns an error. `src` is the expression to use in
-- the error message.
testBool :: Exp -> Ec -> Ec
testBool src k e = isBool e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a bool." (pretty src) (pretty e))

-- | @isNull e@ checks if `e` is null.
isNull :: Ev -> Bool
isNull DNull = True
isNull _ = False

-- | @isCc e@ checks if `e` is a Cc.
isCc :: Ev -> Bool
isCc (DCc _) = True
isCc _ = False

-- | @testCc src k e@ applies `e` to `k` if it is a Cc, otherwise it returns an error. `src` is the expression to use in
-- the error message.
testCc :: Exp -> Ec -> Ec
testCc src k e = isCc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a Cc." (pretty src) (pretty e))
