module Interpreter.Features.For (evalForCom) where

import {-# SOURCE #-} Interpreter.Core.Com
import Interpreter.Core.Exp
import Interpreter.Core.Types
import Interpreter.Helper.Continuation
import Interpreter.Helper.Control
import Interpreter.Helper.TypeTesting
import Parser.Core.Types

-- | @evalForCom com w r c s@ evaluates the for command `com` under the environment `r` and with store `s` then runs the
-- rest of the program `c`.
evalForCom :: Com -> Posn -> Env -> Cc -> Cc
evalForCom (For i1 f1 c1) w r c = evalExp (I i1) (w ! 1) r $ testLoc (I i1) (\l -> evalFor f1 (w ! 2) r (\c' [e] -> update (dvToLoc l) (evalCom c1 (w ! 3) r c') e) c)

-- | @evalFor for w r p c s@ evaluates the for expression `for` under the environment `r` and with store `s` then passes
-- the resulting value and rest of the program `c` into the procedure `p`.
evalFor :: For -> Posn -> Env -> Procedure -> Cc -> Cc
evalFor (ExpFor e1) w r p c = evalRVal e1 (w ! 1) r $ \e -> p c [e]
evalFor (WhileFor e1 e2) w r p c =
  evalRVal
    e1
    (w ! 1)
    r
    ( \e ->
        evalRVal e2 (w ! 2) r $
          testBool
            e2
            (\b -> dvToBool b ?> (p (evalFor (WhileFor e1 e2) w r p c) [e], c))
    )
evalFor (StepFor e1 e2 e3) w r p c = evalRVal e1 (w ! 1) r $ testInt e1 $ step (evalRVal e2 (w ! 2) r, evalRVal e3 (w ! 3) r) p c . dvToInt
  where
    step :: (Ec -> Cc, Ec -> Cc) -> Procedure -> Cc -> Int -> Cc
    step (w1, w2) p c n =
      w1 $
        testInt
          e2
          ( \n1 ->
              w2 $
                testInt
                  e3
                  ( \n2 ->
                      ((n - dvToInt n2) * signum (dvToInt n1) < 1)
                        ?> ( p (step (w1, w2) p c (n + dvToInt n1)) [DInt n],
                             c
                           )
                  )
          )
evalFor (ChainFor f1 f2) w r p c = evalFor f1 (w ! 1) r p $ evalFor f2 (w ! 2) r p c