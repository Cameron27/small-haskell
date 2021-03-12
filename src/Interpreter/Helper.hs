module Interpreter.Helper where

import qualified Data.HashMap.Internal.Strict as HashMap
import Data.Maybe (isNothing)
import Interpreter.Types
  ( Ans (Error),
    Cc,
    Dv (..),
    Ec,
    Env,
    Ev,
    Function,
    Ide,
    Loc,
    Procedure,
    Rv (..),
    Store (..),
    Sv (..),
    inputLoc,
  )

-- Helper Operations --

-- | @cond (x, y) b@ returns `x` if `b` is `True` and `y` if `b` is `False`.
cond :: (a, a) -> Bool -> a
cond (x, _) True = x
cond (_, x) False = x

-- | @b ?> (x, y)@ returns `x` if `b` is `True` and `y` if `b` is `False`.
(?>) :: Bool -> (a, a) -> a
b ?> x = cond x b

-- | `err` is a continuation that always throws an error.
err :: String -> Cc
err err s = Error err

-- Type Conversions --

-- | @svToDv v` returns the denotable value version of `v`.
svToDv :: Sv -> Dv
svToDv (SInt x) = DInt x
svToDv (SDouble x) = DDouble x
svToDv (SString x) = DString x
svToDv (SBool x) = DBool x
svToDv e = error $ "Tried to convert " ++ show e ++ " to a denotable value."

-- | @svToEv v` returns the expressible value version of `v`.
svToEv :: Sv -> Ev
svToEv = svToDv

-- | @dvToSv d` returns the storable value version of `d`.
dvToSv :: Dv -> Sv
dvToSv (DInt x) = SInt x
dvToSv (DDouble x) = SDouble x
dvToSv (DString x) = SString x
dvToSv (DBool x) = SBool x
dvToSv e = error $ "Tried to convert " ++ show e ++ " to a storable value."

-- | @evToSv e` returns the denotable value version of `e`.
evToSv :: Ev -> Sv
evToSv = dvToSv

-- | @dvToRv d` returns the right hand value version of `d`.
dvToRv :: Dv -> Rv
dvToRv (DInt x) = RInt x
dvToRv (DDouble x) = RDouble x
dvToRv (DBool x) = RBool x
dvToRv (DString x) = RString x
dvToRv e = error $ "Tried to convert " ++ show e ++ " to a right hand value."

-- | @evToRv e` returns the right hand value version of `e`.
evToRv :: Ev -> Rv
evToRv = dvToRv

-- | @svToEv e` returns the denotable value version of `e`.
rvToDv :: Rv -> Dv
rvToDv (RInt x) = DInt x
rvToDv (RDouble x) = DDouble x
rvToDv (RBool x) = DBool x
rvToDv (RString x) = DString x

-- | @svToEv e` returns the expressible value version of `e`.
rvToEv :: Rv -> Ev
rvToEv = rvToDv

-- | @ecToLoc d` returns the location `d` represents.
dvToLoc :: Dv -> Loc
dvToLoc (DLoc x) = x
dvToLoc e = error $ "Tried to convert " ++ show e ++ " to a location."

-- | @ecToLoc e` returns the location `e` represents.
evToLoc :: Ev -> Loc
evToLoc = dvToLoc

-- | @dvToFunc d` returns the function `d` represents.
dvToFunc :: Dv -> Function
dvToFunc (DFunc x) = x
dvToFunc e = error $ "Tried to convert " ++ show e ++ " to a function."

-- | @evToFunc e` returns the function `e` represents.
evToFunc :: Ev -> Function
evToFunc = dvToFunc

-- | @dvToFunc d` returns the procedure `d` represents.
dvToProc :: Dv -> Procedure
dvToProc (DProc x) = x
dvToProc e = error $ "Tried to convert " ++ show e ++ " to a procedure."

-- | @evToFunc e` returns the procedure `e` represents.
evToProc :: Ev -> Procedure
evToProc = dvToProc

-- | @dvToBool d` returns the bool `d` represents.
dvToBool :: Dv -> Bool
dvToBool (DBool x) = x
dvToBool e = error $ "Tried to convert " ++ show e ++ " to a function."

-- | @evToBool e` returns the bool `e` represents.
evToBool :: Ev -> Bool
evToBool = dvToBool

-- Type Checks --

-- | @isLoc d@ checks if `d` is a location.
isLoc :: Dv -> Bool
isLoc (DLoc _) = True
isLoc _ = False

-- | @testLoc k e@ applies `e` to `k` if it is a location, otherwise it returns an error.
testLoc :: Ec -> Ec
testLoc k e = isLoc e ?> (k e, err $ show e ++ " is not a location")

-- | @isRv e@ checks if `e` is a right hand value.
isRv :: Ev -> Bool
isRv (DInt _) = True
isRv (DDouble _) = True
isRv (DBool _) = True
isRv (DString _) = True
isRv _ = False

-- | @testRv k e@ applies `e` to `k` if it is a right hand value, otherwise it returns an error.
testRv :: Ec -> Ec
testRv k e = isRv e ?> (k e, err $ show e ++ " is not a right hand value")

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

-- | @testFunc k e@ applies `e` to `k` if it is a function, otherwise it returns an error.
testFunc :: Ec -> Ec
testFunc k e = isFunc e ?> (k e, err $ show e ++ " is not a function")

-- | @isProc e@ checks if `e` is a procedure.
isProc :: Ev -> Bool
isProc (DProc _) = True
isProc _ = False

-- | @testProc k e@ applies `e` to `k` if it is a procedure, otherwise it returns an error.
testProc :: Ec -> Ec
testProc k e = isProc e ?> (k e, err $ show e ++ " is not a procedure")

-- | @isBool e@ checks if `e` is a bool.
isBool :: Ev -> Bool
isBool (DBool _) = True
isBool _ = False

-- | @testBool k e@ applies `e` to `k` if it is a bool, otherwise it returns an error.
testBool :: Ec -> Ec
testBool k e = isBool e ?> (k e, err $ show e ++ " is not a bool")

-- Store Functions --

-- | @isUnused l s@ returns `True` iff the `l` is unused in `s`.
isUnusedS :: Loc -> Store -> Bool
isUnusedS l (Store s _) = isNothing $ HashMap.lookup l s

-- | @lookupS l s@ returns the value that is stored in `s` at `l`.
lookupS :: Loc -> Store -> Sv
lookupS l (Store s _) = case HashMap.lookup l s of
  Just s -> s
  Nothing -> error $ "Tried to lookup location " ++ show l ++ " which has no value assigned to it."

-- | @updateS l v s@ returns a store with `l` updated to `v`.
updateS :: Loc -> Sv -> Store -> Store
updateS l v (Store s x) = Store (HashMap.insert l v s) x

-- | @newLocS s@ returns an unused location in s.
newLocS :: Store -> (Loc, Store)
newLocS (Store s x) = (x, Store s (x + 1))

-- | @nullInput s@ returns `True` iff the input in store `s` is empty.
nullInputS :: Store -> Bool
nullInputS s = case lookupS inputLoc s of
  (SFile []) -> True
  (SFile _) -> False
  _ -> error "A file was not at the input location."

-- Environment Functions --

-- | @updateE r' r@ returns an environment which first checks @r'@ then `r` when a lookup is preformed.
updateE :: Env -> Env -> Env
updateE = HashMap.union

-- | @lookupE i r@ returns the value that is assigned to `i` in `r`.
lookupE :: Ide -> Env -> Dv
lookupE i r = case HashMap.lookup i r of
  Just s -> s
  Nothing -> error $ "Tried to lookup identifier " ++ i ++ " which has no value assigned to it."

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
    ?> ( isUnusedS l s ?> (Error $ show l ++ " is unused", k d s),
         Error $ show e ++ " is not a location"
       )
  where
    l = evToLoc e
    d = svToDv $ lookupS l s

-- | @update l c e s@ stores `e` at `l` and passes the resulting store to `c`.
update :: Loc -> Cc -> Ec
update l c e s = isSv e ?> (c (updateS l (evToSv e) s), Error $show e ++ " is not a storable value")

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
