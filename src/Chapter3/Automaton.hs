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
type Rulebook cond = [AutomatonRule cond]

data AutomatonRule cond where
  AutomatonRule :: Eq cond => cond -> NextState -> AutomatonRule cond

appliesTo :: AutomatonRule cond -> cond -> Bool
AutomatonRule cond _ `appliesTo` cond' = cond == cond'

follow :: AutomatonRule cond -> NextState
follow (AutomatonRule _ nextState) = nextState

class Automaton a where
  accepting :: a -> Bool
  readCharacter :: a -> Char -> a
  readString :: a -> String -> a
  a `readString` str = foldl readCharacter a str
  accepts :: a -> String -> Bool
  a `accepts` str = accepting $ a `readString` str

instance Eq (AutomatonRule cond) where
  AutomatonRule cond ns == AutomatonRule cond' ns' = cond == cond' && ns == ns'
