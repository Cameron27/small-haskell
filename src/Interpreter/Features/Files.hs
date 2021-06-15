module Interpreter.Features.Files (evalFileDec, eofFunc, resetFProc, rewriteFProc, getFProc, putFProc) where

import Common.Formatting
import Interpreter.Core.Types
import Interpreter.Helper.Control
import Interpreter.Helper.Env
import Interpreter.Helper.Store
import Interpreter.Helper.TypeTesting
import Parser.Core.Types
import Text.Printf

-- | A `Filestate` is the state of a file.
data Filestate
  = -- | @Filestate es i e@ is a file containing the values `es`, currently at position `i` and with current value `e`.
    Filestate [Ev] Int (Maybe Ev)

-- | @evalFileDec dec w r u s@ evaluates the file declaration `dec` under the environment `r` and with store `s` then
-- passes the resulting environment into the rest of the program `u`.
evalFileDec :: Dec -> Posn -> Env -> Dc -> Cc
evalFileDec (FileDec i1 i2 _) w r u s = u (newEnvMulti [i1, i2] ls) (updateStore l1 (Just $ EFile $ File [] 1 l2) s')
  where
    (ls', s') = newLocsStore 2 s
    ls = map ELoc ls'
    [l1, l2] = ls'
evalFileDec d1 _ _ _ _ = error $ printf "Cannot run evalFileDec with \"%s\"." (pretty d1)

-- | @doFile f c e s@ applies the filestate transformation `f` to `e` with store `s` then runs the rest of the program
-- `c`.
doFile :: (Filestate -> Either String Filestate) -> Cc -> Ec
doFile f c e s =
  if isLoc e -- Check e is location
    then case lookupFile (evToLoc e) s of -- Get file at l
      Right (File es n l) ->
        case lookupBuffer l s of -- Get value in buffer
          Right e' -> case f (Filestate es n e') of -- Apply f
            Right (Filestate es' n' e'') ->
              c (updateStoreMulti [evToLoc e, l] [Just $ EFile $ File es' n' l, e''] s)
            Left err -> putError err
          Left err -> putError err
      Left err -> putError err
    else putError $ printf "\"%s\" is not a location." (pretty e)
  where
    lookupBuffer l s = case lookupStore l s of
      Sv e -> Right $ Just e
      Unassigned -> Right Nothing
      Unused -> Left $ printf "\"%s\" is unused." (pretty l)

-- | @lookupFile l s@ returns either the file at `l` in `s` or an error message.
lookupFile :: Loc -> Store -> Either String File
lookupFile l s =
  case lookupStore l s of
    (Sv (EFile (File es n l))) -> Right (File es n l)
    (Sv notFile) -> Left $ printf "\"%s\" is not a file." (pretty notFile)
    Unassigned -> Left $ printf "\"%s\" is unassigned." (pretty l)
    Unused -> Left $ printf "\"%s\" is unused." (pretty l)

-- | @resetf f@ resets the `Filestate` `f` to the start.
resetf :: Filestate -> Either String Filestate
resetf (Filestate es n e) = null es ?> (Right $ Filestate es 1 Nothing, Right $ Filestate es 1 (Just $ head es))

-- | A procedure that resets the file passed in.
resetFProc :: Procedure
resetFProc c [e1] = doFile resetf c e1
resetFProc _ es = error $ printf "Should only run resetFProc with a single parameter, not %d." (length es)

-- | @rewritef f@ clears the `Filestate` `f`.
rewritef :: Filestate -> Either String Filestate
rewritef (Filestate es n e) = Right $ Filestate [] 1 Nothing

-- | A procedure that rewrites the file passed in.
rewriteFProc :: Procedure
rewriteFProc c [e1] = doFile rewritef c e1
rewriteFProc _ es = error $ printf "Should only run rewriteFProc with a single parameter, not %d." (length es)

-- | @getf f@ gets the next value in the `Filestate` `f`.
getf :: Filestate -> Either String Filestate
getf (Filestate es n e)
  | n > esLength = Left "cannot get value from beyond end of file."
  | n == esLength = Right (Filestate es (n + 1) Nothing)
  | otherwise = Right (Filestate es (n + 1) (Just $ es !! n))
  where
    esLength = length es

-- | A procedure that get the next value of the file passed in.
getFProc :: Procedure
getFProc c [e1] = doFile getf c e1
getFProc _ es = error $ printf "Should only run getFProc with a single parameter, not %d." (length es)

-- | @rewritef f@ put the current value onto the end of the `Filestate` `f`.
putf :: Filestate -> Either String Filestate
putf (Filestate es n e) =
  if n == length es + 1
    then case e of
      Just e -> Right $ Filestate (es ++ [e]) (n + 1) Nothing
      Nothing -> Left "value must be in buffer to put into file."
    else Left "cannot put value into file unless at end of file."

-- | A procedure that puts current value on the end of the file passed in.
putFProc :: Procedure
putFProc c [e1] = doFile putf c e1
putFProc _ es = error $ printf "Should only run putFProc with a single parameter, not %d." (length es)

-- | A function that returns true iff the file passed in is at the end.
eofFunc :: Function
eofFunc k [e1] =
  if isLoc e1
    then
      ( \l s -> case lookupFile (evToLoc l) s of
          Right (File es n _) -> k (EBool $ n > length es) s
          (Left err) -> putError err
      )
        e1
    else err $ printf "\"%s\" is not a location." (pretty e1)
eofFunc k es = error $ printf "Should only run eofFunc with a single parameter, not %d." (length es)