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
data NFA = NFA
  { nfaStates :: NFAConfig
  , nfaAss :: AcceptStates
  , nfaRules :: NFARulebook
  } deriving (Eq, Show)
makeNfa :: [State] -> AcceptStates -> NFARulebook -> NFA
makeNfa states = NFA (Set.fromList states)

nextStates :: NFA -> Maybe Char -> NFAConfig
nextStates (NFA states _ rules) char = Set.fromList $ follow <$> matchRules
  where
    matchRules = rules `rulesFor` conds
    conds = (, char) <$> (Set.toList states)
    rules `rulesFor` conds = filter match rules
      where match rule = not . null . filter (rule `appliesTo`) $ conds
followFreeMoves :: NFA -> NFA
followFreeMoves nfa = if noMoreStates then nfa else followFreeMoves nfa'
  where states = nfaStates nfa
        moreStates = nextStates nfa Nothing
        noMoreStates = moreStates `Set.isSubsetOf` states
        nfa' = nfa { nfaStates = states <> moreStates }

instance Automaton NFA where
  accepting nfa = not . null $ states' `Set.intersection` (Set.fromList ass)
    where (NFA states' ass _) = followFreeMoves nfa
  nfa `readCharacter` char = nfa { nfaStates = states' }
    where states' = nextStates (followFreeMoves nfa) (Just char)

type NFARule = AutomatonRule NFACond
nfaRule :: NFACond -> NextState -> NFARule
nfaRule = AutomatonRule

instance Show NFARule where
  show (AutomatonRule (s, c) ns) = [i|#<NFARule #{s} \-- #{c} --> #{ns}>|]
