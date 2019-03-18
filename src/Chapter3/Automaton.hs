{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Chapter3.Automaton
  ( AutomatonRule(..)
  , appliesTo, follow
  , Automaton(..)
  , State, NextState, AcceptStates
  ) where

type State = Int
type NextState = State
type AcceptStates = [State]
type Rulebook cond follow = [AutomatonRule cond follow]

data AutomatonRule cond follow where
  AutomatonRule :: (Eq cond, Eq follow)
    => cond -> follow -> AutomatonRule cond follow

appliesTo :: AutomatonRule cond follow -> cond -> Bool
AutomatonRule cond _ `appliesTo` cond' = cond == cond'

follow :: AutomatonRule cond follow -> follow
follow (AutomatonRule _ follow) = follow

class Automaton a where
  accepting :: a -> Bool
  readCharacter :: a -> Char -> a
  readString :: a -> String -> a
  a `readString` str = foldl readCharacter a str
  accepts :: a -> String -> Bool
  a `accepts` str = accepting $ a `readString` str

instance Eq (AutomatonRule cond follow) where
  AutomatonRule cond ns == AutomatonRule cond' ns' = cond == cond' && ns == ns'
