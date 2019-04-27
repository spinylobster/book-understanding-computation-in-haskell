{-# LANGUAGE ViewPatterns #-}

module Chapter9.Type where

import Chapter2.SIMPLE

import qualified Data.Map.Strict as Map
import Data.Maybe

data Type = NumberType | BooleanType | Void | Error deriving (Show, Eq)

type Context = Map.Map String Type

class EvalType a where
  evalType :: Context -> a -> Type

instance EvalType Expression where
  evalType _ (Number _) = NumberType
  evalType _ (Boolean _) = BooleanType
  evalType ctx (Add (evalType ctx -> NumberType) (evalType ctx -> NumberType)) = NumberType
  evalType _ (Add _ _) = Error
  evalType ctx (Multiply (evalType ctx -> NumberType) (evalType ctx -> NumberType)) = NumberType
  evalType _ (Multiply _ _) = Error
  evalType ctx (LessThan (evalType ctx -> NumberType) (evalType ctx -> NumberType)) = BooleanType
  evalType _ (LessThan _ _) = Error
  evalType ctx (Variable name) = fromMaybe Error $ ctx Map.!? name

instance EvalType Statement where
  evalType _ DoNothing = Void
  evalType ctx (Sequence (evalType ctx -> Void) (evalType ctx -> Void)) = Void
  evalType ctx (If (evalType ctx -> BooleanType) (evalType ctx -> Void) (evalType ctx -> Void)) = Void
  evalType ctx (While (evalType ctx -> BooleanType) (evalType ctx -> Void)) = Void
  evalType ctx (Assign name (evalType ctx -> valueType)) = if ok then Void else Error
    where varType = fromMaybe Error $ ctx Map.!? name
          ok = isType varType && isType valueType && varType == valueType
          isType t = t /= Error && t /= Void
  evalType _ _ = Error
