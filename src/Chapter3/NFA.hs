{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Chapter3.NFA
  ( NFA(..)
  , NFARulebook
  , makeNfa
  , nfaRule
  , toDfa
  ) where

import Chapter3.DFA
import Chapter3.Automaton

import Data.Maybe (catMaybes)
import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Map as Map
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

toDfa :: NFA -> DFA
toDfa nfa = makeDfa 1 ass rules
  where
    ass = (allStatesMap Map.!) <$> ass'
      where ass' = filter (accepting . setStates nfa) $ Map.keys allStatesMap
            setStates nfa states = nfa { nfaStates = states }
    rules = toDfaRule <$> allRules
      where toDfaRule (from, char, to) = dfaRule (from', char) to'
              where [from', to'] = (allStatesMap Map.!) <$> [from, to]
    initialStates = nfaStates $ followFreeMoves nfa
    (allStates, allRules) = discover $ Set.singleton initialStates
      where
        discover :: Set.Set NFAConfig -> (Set.Set NFAConfig, [(NFAConfig, Char, NFAConfig)])
        discover states = if noMoreFound then (states, rules) else next
          where noMoreFound = moreStates `Set.isSubsetOf` states
                rules = (discover' <$> alphabet) <*> Set.toList states
                moreStates = Set.fromList . (\(_, _, x) -> x) $ unzip3 rules
                next = discover (states <> moreStates)
        discover' char states = (states, char, nfaStates $ nfa')
          where nfa' = nfa { nfaStates = states } `readCharacter` char
    alphabet = nub . catMaybes $ getChar <$> nfaRules nfa
      where getChar (AutomatonRule (_, c) _) = c
    allStatesMap :: Map.Map NFAConfig DFAConfig
    allStatesMap = foldl (insert) initialMap allStates
      where initialMap = Map.singleton initialStates 1
            insert map states = Map.insertWith seq states state map
              where state = maximum map + 1

type NFARule = AutomatonRule NFACond
nfaRule :: NFACond -> NextState -> NFARule
nfaRule = AutomatonRule

instance Show NFARule where
  show (AutomatonRule (s, c) ns) = [i|#<NFARule #{s} \-- #{c} --> #{ns}>|]
