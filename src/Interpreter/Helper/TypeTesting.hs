module Interpreter.Helper.TypeTesting where

import Common.Formatting
import qualified Data.HashMap.Strict as HashMap
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Parser.Core.Types
import Text.Printf

-- | @evToLoc e@ returns the location `e` represents.
evToLoc :: Ev -> Loc
evToLoc (ELoc x) = x
evToLoc e = error $ printf "Tried to convert \"%s\" to a location." (pretty e)

-- | @evToInt e@ returns the integer `e` represents.
evToInt :: Ev -> Int
evToInt (EInt x) = x
evToInt e = error $ printf "Tried to convert \"%s\" to a integer." (pretty e)

-- | @evToDouble e@ returns the double `e` represents.
evToDouble :: Ev -> Double
evToDouble (EDouble x) = x
evToDouble e = error $ printf "Tried to convert \"%s\" to a double." (pretty e)

-- | @evToArray e@ returns the array `e` represents.
evToArray :: Ev -> Array
evToArray (EArray x) = x
evToArray e = error $ printf "Tried to convert \"%s\" to an array." (pretty e)

-- | @evToRecord e@ returns the record `e` represents.
evToRecord :: Ev -> Record
evToRecord (ERecord x) = x
evToRecord e = error $ printf "Tried to convert \"%s\" to an record." (pretty e)

-- | @evToFunc e@ returns the function `e` represents.
evToFunc :: Ev -> Function
evToFunc (EFunc x _) = x
evToFunc e = error $ printf "Tried to convert \"%s\" to a function." (pretty e)

-- | @evToFunc e@ returns the procedure `e` represents.
evToProc :: Ev -> Procedure
evToProc (EProc x _) = x
evToProc e = error $ printf "Tried to convert \"%s\" to a procedure." (pretty e)

-- | @evToClass e@ returns the class `e` represents.
evToClass :: Ev -> Class
evToClass (EClass x) = x
evToClass e = error $ printf "Tried to convert \"%s\" to a class." (pretty e)

-- | @evToObject e@ returns the object `e` represents.
evToObject :: Ev -> Object
evToObject (EObject x) = x
evToObject e = error $ printf "Tried to convert \"%s\" to an object." (pretty e)

-- | @evToBool e@ returns the bool `e` represents.
evToBool :: Ev -> Bool
evToBool (EBool x) = x
evToBool e = error $ printf "Tried to convert \"%s\" to a bool." (pretty e)

-- | @recordToEnv r@ returns the environment `r` represents.
recordToEnv :: Record -> Env
recordToEnv (Record r) = Env r HashMap.empty emptyEc emptyObj

-- | @objectToPublicEnv o@ returns the public environment `o` represents.
objectToPublicEnv :: Object -> Env
objectToPublicEnv (Object r _ _) = Env r HashMap.empty emptyEc emptyObj

-- | @objectToPrivateEnv o@ returns the private environment `o` represents.
objectToPrivateEnv :: Object -> Env
objectToPrivateEnv (Object r1 r2 _) = Env (HashMap.union r2 r1) HashMap.empty emptyEc emptyObj

-- Type Checks --

-- | @isLoc e@ checks if `e` is a location.
isLoc :: Ev -> Bool
isLoc (ELoc _) = True
isLoc _ = False

-- | @testLoc src k e@ applies `e` to `k` if it is a location, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testLoc :: Exp -> Ec -> Ec
testLoc src k e = isLoc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a location." (pretty src) (pretty e))

-- | @isInt e@ checks if `e` is an integer.
isInt :: Ev -> Bool
isInt (EInt _) = True
isInt _ = False

-- | @testInt src k e@ applies `e` to `k` if it is a integer, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testInt :: Exp -> Ec -> Ec
testInt src k e = isInt e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an integer." (pretty src) (pretty e))

-- | @isNumber e@ checks if `e` is a number.
isNumber :: Ev -> Bool
isNumber (EInt _) = True
isNumber (EDouble _) = True
isNumber _ = False

-- | @testNumber src k e@ applies `e` to `k` if it is a number, otherwise it returns an error. `src` is the expression
-- to use in the error message.
testNumber :: Exp -> Ec -> Ec
testNumber src k e = isNumber e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a number." (pretty src) (pretty e))

-- | @isRv e@ checks if `e` is a right hand value.
isRv :: Ev -> Bool
isRv (EInt _) = True
isRv (EDouble _) = True
isRv (EBool _) = True
isRv (EString _) = True
isRv (ELoc _) = True
isRv (EArray _) = True
isRv (ERecord _) = True
isRv (EObject _) = True
isRv ENull = True
isRv _ = False

-- | @testRv src k e@ applies `e` to `k` if it is a right hand value, otherwise it returns an error. `src` is the
-- expression to use in the error message.
testRv :: Exp -> Ec -> Ec
testRv src k e = isRv e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a right hand value." (pretty src) (pretty e))

-- | @isDv e@ checks if `e` is a denotable value.
isDv :: Ev -> Bool
isDv (EInt _) = True
isDv (EDouble _) = True
isDv (EBool _) = True
isDv (EString _) = True
isDv (ELoc _) = True
isDv (EArray _) = True
isDv (ERecord _) = True
isDv (EProc _ _) = True
isDv (EFunc _ _) = True
isDv (EMethod _) = True
isDv (EClass _) = True
isDv (EObject _) = True
isDv ENull = True
isDv (ECc _) = True
isDv _ = False

-- | @testDv src k e@ applies `e` to `k` if it is a denotable value, otherwise it returns an error. `src` is the
-- expression to use in the error message.
testDv :: Exp -> Ec -> Ec
testDv src k e = isDv e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a denotable value." (pretty src) (pretty e))

-- | @isSv e@ checks if `e` is a storable value.
isSv :: Ev -> Bool
isSv (EInt _) = True
isSv (EDouble _) = True
isSv (EBool _) = True
isSv (EString _) = True
isSv (ELoc _) = True
isSv (EArray _) = True
isSv (ERecord _) = True
isSv (EFile _) = True
isSv (EObject _) = True
isSv ENull = True
isSv _ = False

-- | @isProc e@ checks if `e` is a procedure.
isProc :: Ev -> Bool
isProc (EProc _ _) = True
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
    (EProc _ n) = e

-- | @isMethod e@ checks if `e` is a method.
isMethod :: Ev -> Bool
isMethod (EMethod _) = True
isMethod _ = False

-- | @testMethod src k e@ applies `e` to `k` if it is a method, otherwise it returns an error. `src` is the
-- expression to use in the error message.
testMethod :: Exp -> Ec -> Ec
testMethod src k e = isMethod e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a method." (pretty src) (pretty e))

-- | @isFunc e@ checks if `e` is a function.
isFunc :: Ev -> Bool
isFunc (EFunc _ _) = True
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
    (EFunc _ n) = e

-- | @isClass e@ checks if `e` is a class.
isClass :: Ev -> Bool
isClass (EClass _) = True
isClass _ = False

-- | @testClass src k e@ applies `e` to `k` if it is a class, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testClass :: Exp -> Ec -> Ec
testClass src k e = isClass e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a class." (pretty src) (pretty e))

-- | @isObject e@ checks if `e` is an object.
isObject :: Ev -> Bool
isObject (EObject _) = True
isObject _ = False

-- | @testObject src k e@ applies `e` to `k` if it is an object, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testObject :: Exp -> Ec -> Ec
testObject src k e = isObject e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an object." (pretty src) (pretty e))

-- | @isArray e@ checks if `e` is an array.
isArray :: Ev -> Bool
isArray (EArray _) = True
isArray _ = False

-- | @testArray src k e@ applies `e` to `k` if it is an array, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testArray :: Exp -> Ec -> Ec
testArray src k e = isArray e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not an array." (pretty src) (pretty e))

-- | @isRecord e@ checks if `e` is a record.
isRecord :: Ev -> Bool
isRecord (ERecord _) = True
isRecord _ = False

-- | @testRecord src k e@ applies `e` to `k` if it is a record, otherwise it returns an error. `src` is the expression to
-- use in the error message.
testRecord :: Exp -> Ec -> Ec
testRecord src k e = isRecord e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a record." (pretty src) (pretty e))

-- | @isBool e@ checks if `e` is a bool.
isBool :: Ev -> Bool
isBool (EBool _) = True
isBool _ = False

-- | @testBool src k e@ applies `e` to `k` if it is a bool, otherwise it returns an error. `src` is the expression to use in
-- the error message.
testBool :: Exp -> Ec -> Ec
testBool src k e = isBool e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a bool." (pretty src) (pretty e))

-- | @isNull e@ checks if `e` is null.
isNull :: Ev -> Bool
isNull ENull = True
isNull _ = False

-- | @isCc e@ checks if `e` is a Cc.
isCc :: Ev -> Bool
isCc (ECc _) = True
isCc _ = False

-- | @testCc src k e@ applies `e` to `k` if it is a Cc, otherwise it returns an error. `src` is the expression to use in
-- the error message.
testCc :: Exp -> Ec -> Ec
testCc src k e = isCc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a Cc." (pretty src) (pretty e))
