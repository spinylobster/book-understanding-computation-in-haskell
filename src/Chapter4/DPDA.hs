{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Chapter4.DPDA
  ( DPDA
  , makeDpda
  , pdaRule
  , Stack, PopChar, PushChars
  , PDACond, PDAFollow, PDARule, PDARulebook, DPDAConfig
  ) where

import Chapter3.Automaton

import Data.String.Interpolate

type PopChar = Char
type PushChars = [Char]
type DPDAConfig = (State, Stack)
type PDACond = (State, PopChar, Maybe Char)
type PDAFollow = (PushChars, NextState)
type PDARulebook = [PDARule]

type Stack = [Char]

type PDARule = AutomatonRule PDACond PDAFollow
pdaRule :: PDACond -> PDAFollow -> AutomatonRule PDACond PDAFollow
pdaRule = AutomatonRule

instance Show PDARule where
  show (AutomatonRule (state, popC, char) (pushCs, nextState))
    = [i|#{state} --#{char};#{popC}/#{pushCs}--> #{nextState}|]

data DPDA = DPDA
  { dpdaConfig :: DPDAConfig
  , dpdaAss :: AcceptStates
  , dpdaRules :: PDARulebook
  } deriving (Show, Eq)
makeDpda state = DPDA (state, ['$'])

instance Automaton DPDA where
  accepting dpda = state' `elem` dpdaAss dpda
    where (state', _) = dpdaConfig $ followFreeMoves dpda
  readCharacter dpda char = followFreeMoves dpda''
    where dpda' = followFreeMoves dpda
          dpda'' = dpda' { dpdaConfig = nextConfig dpda' (Just char) }

appliableRules :: DPDA -> Maybe Char -> [PDARule]
appliableRules dpda char = filter (`appliesTo` cond) $ dpdaRules dpda
  where cond = (state, top, char)
        (state, top:_) = dpdaConfig dpda

nextConfig :: DPDA -> Maybe Char -> DPDAConfig
nextConfig dpda char = dpdaConfig dpda'
  where dpda' = dpda { dpdaConfig = (nextState, pushChars <> stack') }
          where rule = case appliableRules dpda char of
                  [r] -> r
                  _ -> pdaRule (-1, '_', Nothing) ("invalid", -1)
                (_, _:stack') = dpdaConfig dpda
                (pushChars, nextState) = follow rule

followFreeMoves :: DPDA -> DPDA
followFreeMoves dpda = if appliable then followFreeMoves dpda' else dpda
  where appliable = not . null $ appliableRules dpda Nothing
        dpda' = dpda { dpdaConfig = config' }
        config' = dpda `nextConfig` Nothing
