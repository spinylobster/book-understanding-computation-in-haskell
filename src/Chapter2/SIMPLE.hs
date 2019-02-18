{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Chapter2.SIMPLE
  ( SIMPLE(..)
  , Expression(..)
  , Statement(..)
  , Env
  , inspect
  , run
  ) where

import Data.Char
import qualified Data.Map.Strict as Map

type Env = Map.Map String Expression
type Machine a = (a, Env)

class Show a => SIMPLE a where
  reducible :: a -> Bool
  reduce :: Machine a -> Machine a
  evaluate :: Machine a -> Machine a
  toRuby :: a -> String

run :: SIMPLE a => Machine a -> IO ()
run (program, env) = do
  if reducible program then do
    print program
    run $ reduce (program, env)
  else print program

data Expression =
  Variable String
  | Number Integer
  | Add Expression Expression
  | Multiply Expression Expression
  | Boolean Bool
  | And Expression Expression
  | Or Expression Expression
  | LessThan Expression Expression
  deriving (Eq)

instance Show Expression where
  show (Variable name) = name
  show (Number a) = show a
  show (Add left right) = show left ++ " + " ++ show right
  show (Multiply left right) = show left ++ " * " ++ show right
  show (Boolean b) = show b
  show (And left right) = show left ++ " && " ++ show right
  show (Or left right) = show left ++ " || " ++ show right
  show (LessThan left right) = show left ++ " < " ++ show right

instance SIMPLE Expression where
  reducible (Number _) = False
  reducible (Boolean _) = False
  reducible _ = True

  reduce (exp, env) = (reduce' exp, env)
    where
      reduce' (Variable name) = env Map.! name
      reduce' (Add (Number left) (Number right)) = Number $ left + right
      reduce' (Add left right) = Add left' right'
        where (left', right') = reduceFromLeft env (left, right)
      reduce' (Multiply (Number left) (Number right)) = Number $ left * right
      reduce' (Multiply left right) = Multiply left' right'
        where (left', right') = reduceFromLeft env (left, right)
      reduce' (LessThan (Number left) (Number right)) = Boolean $ left < right
      reduce' (LessThan left right) = LessThan left' right'
        where (left', right') = reduceFromLeft env (left, right)

  evaluate (exp, env) = (evaluate' exp, env)
    where
      evalF = fst . evaluate . (, env)
      evaluate' (Variable name) = env Map.! name
      evaluate' self@(Number _) = self
      evaluate' (Add left right) = Number $ left' + right'
        where [Number left', Number right'] = evalF <$> [left, right]
      evaluate' (Multiply left right) = Number $ left' * right'
        where [Number left', Number right'] = evalF <$> [left, right]
      evaluate' self@(Boolean _) = self
      evaluate' (LessThan left right) = Boolean $ left' < right'
        where [Number left', Number right'] = evalF <$> [left, right]

  toRuby (Number value) = "-> e { " <> show value <> " }"
  toRuby (Boolean value) = "-> e { " <> (toLower <$> show value) <> " }"
  toRuby (Variable name) = "-> e { e[:" <> name <> "] }"
  toRuby (Add left right) = "-> e { (" <> toRuby left <> ").call(e) + (" <> toRuby right <> ").call(e) }"
  toRuby (Multiply left right) = "-> e { (" <> toRuby left <> ").call(e) * (" <> toRuby right <> ").call(e) }"
  toRuby (LessThan left right) = "-> e { (" <> toRuby left <> ").call(e) < (" <> toRuby right <> ").call(e) }"

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
  reducible (DoNothing) = False
  reducible _ = True

  reduce ((Assign name exp@(reducible -> False)), env) = (DoNothing, env')
    where env' = Map.insert name exp env
  reduce ((Assign name exp@(reducible -> True)), env) = (Assign name exp', env)
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

inspect :: SIMPLE a => a -> String
inspect exp = "<<" <> show exp <> ">>"

reduceFromLeft :: SIMPLE a => Env -> (a, a) -> (a, a)
reduceFromLeft env (left@(reducible -> True), right) = (left', right)
  where (left', env') = reduce (left, env)
reduceFromLeft env (left, right@(reducible -> True)) = (left, right')
  where (right', env') = reduce (right, env)
