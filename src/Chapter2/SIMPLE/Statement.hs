{-# LANGUAGE ViewPatterns #-}

module Chapter2.SIMPLE.Statement
  ( Statement(..)
  ) where

import Chapter2.SIMPLE.Expression

import qualified Data.Map.Strict as Map

data Statement =
    DoNothing
  | Assign String Expression
  | If Expression Statement Statement
  | Sequence Statement Statement
  | While Expression Statement
  deriving (Eq)

instance Show Statement where
  show DoNothing = "do-nothing"
  show (Assign name exp) = name ++ " = " ++ show exp
  show (If cond cons alt) = "if (" ++ show cond ++ ") { " ++ show cons ++ " } else { " ++ show alt ++ " }"
  show (Sequence first second) = show first ++ "; " ++ show second
  show (While cond body) = "while (" ++ show cond ++ ") { " ++ show body ++ " }"

instance SIMPLE Statement where
  reducible DoNothing = False
  reducible _ = True

  reduce (Assign name exp@(reducible -> False), env) = (DoNothing, env')
    where env' = Map.insert name exp env
  reduce (Assign name exp@(reducible -> True), env) = (Assign name exp', env)
    where exp' = fst . reduce $ (exp, env)
  reduce (If (Boolean True) cons _, env) = (cons, env)
  reduce (If (Boolean False) _ alt, env) = (alt, env)
  reduce (If cond@(reducible -> True) cons alt, env) = (stat', env)
    where stat' = If (fst . reduce $ (cond, env)) cons alt
  reduce (Sequence DoNothing second, env) = (second, env)
  reduce (Sequence first second, env) = (Sequence first' second, env')
    where (first', env') = reduce (first, env)
  reduce (stat@(While cond body), env) = (stat', env)
    where stat' = If cond (Sequence body stat) DoNothing

  evaluate self@(DoNothing, env) = self
  evaluate ((Assign name exp), env) = (DoNothing, env')
    where env' = Map.insert name value env
          value = fst . evaluate $ (exp, env)
  evaluate (If cond cons alt, env) = if cond' then cons' else alt'
    where Boolean cond' = fst . evaluate $ (cond, env)
          cons' = evaluate (cons, env)
          alt' = evaluate (alt, env)
  evaluate (Sequence first second, env) = evaluate (second, env')
    where env' = snd . evaluate $ (first, env)
  evaluate (stat@(While cond body), env) = evaluate (stat', env')
    where Boolean cond' = fst . evaluate $ (cond, env)
          stat' = if cond' then stat else DoNothing
          env' = if cond' then snd . evaluate $ (body, env) else env

  toRuby DoNothing = "-> e { e }"
  toRuby (Assign name value) = "-> e { e.merge({ :" <> name <> " => (" <> toRuby value <> ").call(e) }) }"
  toRuby (If cond cons alt) = "-> e { if (" <> toRuby cond <> ").call(e) then " <> toRuby cons <> " else " <> toRuby alt <> " end }"
  toRuby (Sequence first second) = "-> e { " <> toRuby second <> ".call(" <> toRuby first <> ".call(e)) }"
  toRuby (While cond body) = "-> e { while (" <> toRuby cond <> ").call(e); e = (" <> toRuby body <> ").call(e); end; e }"

reduceFromLeft :: SIMPLE a => Env -> (a, a) -> (a, a)
reduceFromLeft env (left@(reducible -> True), right) = (left', right)
  where (left', env') = reduce (left, env)
reduceFromLeft env (left, right@(reducible -> True)) = (left, right')
  where (right', env') = reduce (right, env)
