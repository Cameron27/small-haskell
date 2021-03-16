module Interpreter.Helper where

import Classes
import qualified Data.HashMap.Internal.Strict as HashMap
import Data.Maybe
import Interpreter.Types
import Parser.Types
import System.Exit
import System.IO
import Text.Printf

-- Helper Operations --

-- | @cond (x, y) b@ returns `x` if `b` is `True` and `y` if `b` is `False`.
cond :: (a, a) -> Bool -> a
cond (x, _) True = x
cond (_, x) False = x

-- | @b ?> (x, y)@ returns `x` if `b` is `True` and `y` if `b` is `False`.
(?>) :: Bool -> (a, a) -> a
b ?> x = cond x b

-- | @putError err@ outputs the error message `err`.
putError :: String -> Ans
putError err = hPutStrLn stderr ("Error: " ++ err) >> return (ExitFailure 1)

-- | `err` is a continuation that always throws an error.
err :: String -> Cc
err err s = putError err >> return (ExitFailure 1)

-- Type Conversions --

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

-- Store Functions --

-- | @isUnused l s@ returns `True` iff the `l` is unused in `s`.
isUnusedS :: Loc -> Store -> Bool
isUnusedS l (Store s _) = isNothing $ HashMap.lookup l s

-- | @lookupS l s@ returns the value that is stored in `s` at `l`.
lookupS :: Loc -> Store -> Sv
lookupS l (Store s _) = case HashMap.lookup l s of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup location \"Loc(%d)\" which has no value assigned to it." l

-- | @updateS l v s@ returns a store with `l` updated to `v`.
updateS :: Loc -> Sv -> Store -> Store
updateS l v (Store s x) = Store (HashMap.insert l v s) x

-- | @newLocS s@ returns an unused location in s.
newLocS :: Store -> (Loc, Store)
newLocS (Store s x) = (x, Store s (x + 1))

-- Environment Functions --

-- | @updateE r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed.
updateE :: Env -> Env -> Env
updateE = HashMap.union

-- | @lookupE i r@ returns the value that is assigned to `i` in `r`.
lookupE :: Ide -> Env -> Dv
lookupE i r = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ printf "Tried to lookup identifier \"%s\" which has no value assigned to it." i

-- | @newE i d@ returns a new environment with just `i` bound to `d`.
newE :: Ide -> Dv -> Env
newE i l = HashMap.fromList [(i, l)]

-- | @isUnboundE i r@ is `True` iff `i` is unbound in `r`.
isUnboundE :: Ide -> Env -> Bool
isUnboundE i r = case HashMap.lookup i r of
  Just _ -> False
  Nothing -> True

-- Other Functions --

-- | @cont k e s@ looks up the location `e` in `s` and passes the result, along with `s` into `k`.
cont :: Ec -> Ec
cont k e s =
  isLoc e
    ?> ( isUnusedS l s ?> (error $ printf "\"Loc(%d)\" is unused." l, k d s),
         error $ printf "\"%s\" is not a location." (pretty e)
       )
  where
    l = evToLoc e
    d = svToDv $ lookupS l s

-- | @update l c e s@ stores `e` at `l` and passes the resulting store to `c`.
update :: Loc -> Cc -> Ec
update l c e s = isSv e ?> (c (updateS l (evToSv e) s), putError $ error $ printf "Tried to store the value \"%s\" which is not storable." (pretty e))

-- | @ref k e s@ gets an unused location in `s`, updates it with `e` then passes it, along with the updated store, to
-- `k`.
ref :: Ec -> Ec
ref k e s = update newLoc (k (DLoc newLoc)) e s'
  where
    (newLoc, s') = newLocS s

-- | @deref k e s@ checks if `e` is a location. If it is then the content of `e`, along with `s`, are passed to `k`,
-- otherwise it just passes `e` and `s` to `k`.
deref :: Ec -> Ec
deref k e s = isLoc e ?> (cont k e s, k e s)
