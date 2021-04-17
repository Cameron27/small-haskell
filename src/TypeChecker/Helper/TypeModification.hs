{-# LANGUAGE LambdaCase #-}

module TypeChecker.Helper.TypeModification where

import Common.Formatting
import Data.List
import qualified Data.Set as Set
import Parser.Types
import Text.Printf
import TypeChecker.Types

ref :: Type -> Type
ref TAny = TAny
ref TJump = TJump
ref (TSet ts) = reduceType $ TSet $ Set.map ref ts
ref t = TRef t

deref :: Type -> Type
deref (TRef t) = t
deref (TSet ts) = reduceType $ TSet $ Set.map deref ts
deref t = t

rval :: Pretty a => a -> Type -> Either TypeError Type
rval src t =
  let t' = deref t
   in if t' `leq` rTypes
        then Right t'
        else Left $ TypeError $ printf "\"%s\" is not a right hand value in \"%s\"" (show t') (pretty src)
  where
    rTypes = TSet (Set.fromList [TInt, TDouble, TBool, TString, TRef TAny, TArray TAny, TAnyRecord])

createTSet :: [Type] -> Type
createTSet ts = reduceType $ TSet $ Set.fromList ts'
  where
    (sets', notSets) =
      partition
        ( \case
            TSet _ -> True
            _ -> False
        )
        ts
    sets = map (\(TSet ts) -> Set.toList ts) sets'
    ts' = concat $ notSets : sets

reduceType :: Type -> Type
reduceType (TSet set) = simplifySet $ removeRedundant set
  where
    removeRedundant set = Set.filter (\t1 -> not $ any (\t2 -> t1 `leq` t2 && t1 /= t2) set) set
    simplifySet set =
      if Set.size set == 1
        then Set.elemAt 0 set
        else TSet set
reduceType t = t

dearray :: Type -> Type
dearray (TArray t) = t
dearray (TSet set) = TSet $ Set.map dearray set
dearray t = error $ printf "Cannot dearray %s." (show t)