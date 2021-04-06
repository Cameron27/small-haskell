module Interpreter.FileOperations (evalEof, evalResetF, evalRewriteF, evalGetF, evalPutF) where

import Common.Formatting
import Debug.Trace
import Interpreter.Helper.Control
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Interpreter.Types
import Parser.Types
import Text.Printf

data Filestate = Filestate [Rv] Integer (Maybe Rv)

evalEof :: (Exp -> Env -> Ec -> Cc) -> Exp -> Env -> Ec -> Cc
evalEof evalExp e1 r k =
  evalExp e1 r $
    testLoc
      e1
      ( \l s ->
          if not $ isUnusedStore (dvToLoc l) s
            then case svToDv $ lookupStore (dvToLoc l) s of
              (DFile (File es n _)) -> k (DBool (n > toInteger (length es))) s
              notFile -> putError $ printf "\"%s\", evaluated as \"%s\", is not a file." (pretty e1) (pretty notFile)
            else putError $ printf "\"%s\" is unbound." (pretty l)
      )

doFile :: Exp -> (Filestate -> Either String Filestate) -> Cc -> Ec
doFile src f c e s =
  if isLoc e -- check e is a location
    then
      if not (isUnusedStore (dvToLoc e) s) -- check e is bound to something
        then case svToDv $ lookupStore (dvToLoc e) s of -- check if the value at location e is a file
          (DFile (File es n l)) ->
            let e' = if isUnusedStore l s then Nothing else Just $ svToRv $ lookupStore l s -- get current value in file buffer
             in case f (Filestate es n e') of -- apply file state change function
                  Right (Filestate es' n' e'') -> c (updateStoreMulti [dvToLoc e, l] [Just $ SFile $ File es' n' l, rvToSv <$> e''] s)
                  Left err -> putError err
          notFile -> putError $ printf "\"%s\", evaluated as \"%s\", is not a file." (pretty src) (pretty notFile)
        else putError $ printf "\"%s\" is unbound." (pretty e)
    else putError $ printf "\"%s\", evaluated as \"%s\", is not a location." (pretty src) (pretty e)

resetf :: Filestate -> Either String Filestate
resetf (Filestate es n e) = null es ?> (Right $ Filestate es 1 Nothing, Right $ Filestate es 1 (Just $ head es))

evalResetF :: (Exp -> Env -> Ec -> Cc) -> Exp -> Env -> Cc -> Cc
evalResetF evalExp e1 r c = evalExp e1 r $ doFile e1 resetf c

rewritef :: Filestate -> Either String Filestate
rewritef (Filestate es n e) = Right $ Filestate [] 1 Nothing

evalRewriteF :: (Exp -> Env -> Ec -> Cc) -> Exp -> Env -> Cc -> Cc
evalRewriteF evalExp e1 r c = evalExp e1 r $ doFile e1 rewritef c

getf :: Filestate -> Either String Filestate
getf (Filestate es n e)
  | n > esLength = Left "cannot get value from beyond end of file."
  | n == esLength = Right (Filestate es (n + 1) Nothing)
  | n < esLength = Right (Filestate es (n + 1) (Just $ es !! fromInteger n))
  where
    esLength = toInteger $ length es

evalGetF :: (Exp -> Env -> Ec -> Cc) -> Exp -> Env -> Cc -> Cc
evalGetF evalExp e1 r c = evalExp e1 r $ doFile e1 getf c

putf :: Filestate -> Either String Filestate
putf (Filestate es n e) =
  if n == toInteger (length es) + 1
    then case e of
      Just e -> Right $ Filestate (es ++ [e]) (n + 1) Nothing
      Nothing -> Left "value must be in buffer to put into file."
    else Left "cannot put value into file unless at end of file."

evalPutF :: (Exp -> Env -> Ec -> Cc) -> Exp -> Env -> Cc -> Cc
evalPutF evalExp e1 r c = evalExp e1 r $ doFile e1 putf c
