{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter3.DFA where

import Chapter3.Automaton

import Data.String.Interpolate

type DFAConfig = State
type DFACond = (DFAConfig, Char)
type DFARulebook = [DFARule]

data DFA = DFA DFAConfig AcceptStates DFARulebook deriving (Eq, Show)
makeDfa :: State -> AcceptStates -> DFARulebook -> DFA
makeDfa = DFA

instance Automaton DFA where
  accepting (DFA state ass _) = state `elem` ass
  DFA state ass rules `readCharacter` char = DFA state' ass rules
    where state' = nextState state char rules
          nextState state char rules = follow $ rules `ruleFor` cond
            where cond = (state, char)
                  rules `ruleFor` cond = head . filter (`appliesTo` cond) $ rules

type DFARule = AutomatonRule DFACond NextState
dfaRule :: DFACond -> NextState -> DFARule
dfaRule = AutomatonRule

instance Show DFARule where
  show (AutomatonRule (s, c) ns) = [i|#<DFARule #{s} \-- #{c} --> #{ns}>|]
