{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Chapter3.NFA where

import Chapter3.DFA
import Chapter3.Automaton

import qualified Data.Set as Set
import Data.String.Interpolate

type NFAConfig = Set.Set State

type NFACond = (State, Maybe Char)
type NFARulebook = [NFARule]
data NFA = NFA NFAConfig AcceptStates NFARulebook deriving (Eq, Show)
makeNfa :: [State] -> AcceptStates -> NFARulebook -> NFA
makeNfa states = NFA (Set.fromList states)

instance Automaton NFA where
  accepting (NFA states ass _) = not . null $ states `Set.intersection` (Set.fromList ass)
  NFA states ass rules `readCharacter` char = NFA states' ass rules
    where states' = nextStates (followFreeMoves states) $ Just char
          followFreeMoves states =
            let moreStates = nextStates states Nothing
            in if moreStates `Set.isSubsetOf` states
              then states else followFreeMoves $ states `Set.union` moreStates
          nextStates states char = Set.fromList $ follow <$> matchRules
            where
              matchRules = rules `rulesFor` conds
              conds = (, char) <$> (Set.toList states)
              rules `rulesFor` conds = filter match rules
                where match rule = not . null . filter (rule `appliesTo`) $ conds

type NFARule = AutomatonRule NFACond
nfaRule :: NFACond -> NextState -> NFARule
nfaRule = AutomatonRule

instance Show NFARule where
  show (AutomatonRule (s, c) ns) = [i|#<NFARule #{s} \-- #{c} --> #{ns}>|]
