{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Chapter2.SIMPLE.Expression
  ( SIMPLE(..)
  , Expression(..)
  , Env
  , Machine
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
run (program, env) =
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
  show (Boolean b) = toLower <$> show b
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

inspect :: SIMPLE a => a -> String
inspect exp = "<<" <> show exp <> ">>"

reduceFromLeft :: SIMPLE a => Env -> (a, a) -> (a, a)
reduceFromLeft env (left@(reducible -> True), right) = (left', right)
  where (left', env') = reduce (left, env)
reduceFromLeft env (left, right@(reducible -> True)) = (left, right')
  where (right', env') = reduce (right, env)
