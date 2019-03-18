{-# LANGUAGE TupleSections #-}

module Chapter4.NPDA
  ( NPDA
  , makeNpda
  ) where

import Chapter3.Automaton
import Chapter4.DPDA

import qualified Data.Set as Set

type NPDAConfig = Set.Set DPDAConfig

data NPDA = NPDA
  { npdaConfig :: NPDAConfig
  , npdaAss :: AcceptStates
  , npdaRules :: PDARulebook
  } deriving (Show, Eq)
makeNpda = NPDA (Set.singleton (1, ['$']))

instance Automaton NPDA where
  accepting npda = not . null $ states' `Set.intersection` (Set.fromList $ npdaAss npda)
    where states' = Set.map fst $ npdaConfig $ followFreeMoves npda
  readCharacter npda char = followFreeMoves . readChar' . followFreeMoves $ npda
    where readChar' npda = npda { npdaConfig = nextConfigs npda (Just char) }

matchWith :: PDARule -> (DPDAConfig, Maybe Char) -> Bool
rule `matchWith` ((state, top:_), char) = rule `appliesTo` cond
  where cond = (state, top, char)

nextConfigs :: NPDA -> Maybe Char -> NPDAConfig
nextConfigs (NPDA configs _ rules) char = configs'
  where
    configs' = Set.fromList $ calcNextConfig <$> ruleAndConfigs
    calcNextConfig (rule, (_, _:popped)) = (nextState, pushChars <> popped)
      where (pushChars, nextState) = follow rule
    ruleAndConfigs :: [(PDARule, DPDAConfig)]
    ruleAndConfigs = rules >>= (calcRuleAndConfig configs)
    calcRuleAndConfig configs rule = (rule,) <$> appliableConfigs
      where appliableConfigs = fst <$> matched
              where matched = filter (rule `matchWith`) conds
                    conds = (,char) <$> Set.toList configs

followFreeMoves :: NPDA -> NPDA
followFreeMoves npda = if noMoreConfigs then npda else followFreeMoves npda'
  where (NPDA configs _ rules) = npda
        noMoreConfigs = configs' `Set.isSubsetOf` configs
        npda' = npda { npdaConfig = configs <> configs' }
        configs' = npda `nextConfigs` Nothing
