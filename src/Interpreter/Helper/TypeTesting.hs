module Interpreter.Helper.TypeTesting where

-- Type Conversions --

import Common.Formatting
import Interpreter.Helper.Control
import Interpreter.Types
import Parser.Types
import Text.Printf

-- | @svToDv v` returns the denotable value version of `v`.
svToDv :: Sv -> Dv
svToDv (SInt x) = DInt x
svToDv (SDouble x) = DDouble x
svToDv (SString x) = DString x
svToDv (SBool x) = DBool x

-- | @svToEv v` returns the expressible value version of `v`.
svToEv :: Sv -> Ev
svToEv = svToDv

-- | @dvToSv d` returns the storable value version of `d`.
dvToSv :: Dv -> Sv
dvToSv (DInt x) = SInt x
dvToSv (DDouble x) = SDouble x
dvToSv (DString x) = SString x
dvToSv (DBool x) = SBool x
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
dvToRv e = error $ printf "Tried to convert \"%s\" to a right hand value." (pretty e)

-- | @evToRv e` returns the right hand value version of `e`.
evToRv :: Ev -> Rv
evToRv = dvToRv

-- | @ecToLoc d` returns the location `d` represents.
dvToLoc :: Dv -> Loc
dvToLoc (DLoc x) = x
dvToLoc e = error $ printf "Tried to convert \"%s\" to a location." (pretty e)

-- | @ecToLoc e` returns the location `e` represents.
evToLoc :: Ev -> Loc
evToLoc = dvToLoc

-- | @dvToFunc d` returns the function `d` represents.
dvToFunc :: Dv -> Function
dvToFunc (DFunc x) = x
dvToFunc e = error $ printf "Tried to convert \"%s\" to a function." (pretty e)

-- | @evToFunc e` returns the function `e` represents.
evToFunc :: Ev -> Function
evToFunc = dvToFunc

-- | @dvToFunc d` returns the procedure `d` represents.
dvToProc :: Dv -> Procedure
dvToProc (DProc x) = x
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
testLoc e1 k e = isLoc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a variable." (pretty e1) (pretty e))

-- | @isRv e@ checks if `e` is a right hand value.
isRv :: Ev -> Bool
isRv (DInt _) = True
isRv (DDouble _) = True
isRv (DBool _) = True
isRv (DString _) = True
isRv _ = False

-- | @testRv e1 k e@ applies `e` to `k` if it is a right hand value, otherwise it returns an error. `e1` is the
-- expression to use in the error message.
testRv :: Exp -> Ec -> Ec
testRv e1 k e = isRv e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", a right hand value." (pretty e1) (pretty e))

-- | @isSv e@ checks if `e` is a storable value.
isSv :: Ev -> Bool
isSv (DInt _) = True
isSv (DDouble _) = True
isSv (DBool _) = True
isSv (DString _) = True
isSv _ = False

-- | @isFunc e@ checks if `e` is a function.
isFunc :: Ev -> Bool
isFunc (DFunc _) = True
isFunc _ = False

-- | @testFunc e1 k e@ applies `e` to `k` if it is a function, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testFunc :: Exp -> Ec -> Ec
testFunc e1 k e = isFunc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a function." (pretty e1) (pretty e))

-- | @isProc e@ checks if `e` is a procedure.
isProc :: Ev -> Bool
isProc (DProc _) = True
isProc _ = False

-- | @testProc e1 k e@ applies `e` to `k` if it is a procedure, otherwise it returns an error. `e1` is the expression to
-- use in the error message.
testProc :: Exp -> Ec -> Ec
testProc e1 k e = isProc e ?> (k e, err $ printf "\"%s\", evaluated as \"%s\", is not a procedure." (pretty e1) (pretty e))

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