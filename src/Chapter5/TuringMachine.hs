{-# LANGUAGE QuasiQuotes #-}

module Chapter5.TuringMachine
  ( Tape(..), mkRightStartTape, mkLeftStartTape
  , HeadMoveDirection(..), moveHead, write
  , TMRule(..), appliesTo, follow
  , TMConfig, nextConfig
  , DTM(..), accepting, step, run
  ) where

import Data.Maybe
import Data.String.Interpolate

data Tape = Tape
  { left :: [Maybe Char]
  , middle :: Maybe Char
  , right :: [Maybe Char] }

instance Show Tape where
  show (Tape left middle right) = [i|_#{leftStr}#{middle'}#{rightStr}_|]
    where middle' = case middle of Nothing -> "(_)"; Just char -> '(':char:")"
          leftStr = reverse $ fromJust <$> takeWhile isJust left
          rightStr = fromJust <$> takeWhile isJust right

instance Eq Tape where
  (Tape l m r) == (Tape l' m' r') = m == m' && left && right
    where left = takeJust l == takeJust l'
          right = takeJust r == takeJust r'
          takeJust = takeWhile isJust

mkRightStartTape str = Tape (left ++ repeat Nothing) middle right
  where left = reverse $ take (length str - 1) str'
        middle = last str'
        right = repeat Nothing
        str' = Just <$> str

mkLeftStartTape str = Tape left middle (right ++ repeat Nothing)
  where left = repeat Nothing
        middle = head str'
        right = drop 1 str'
        str' = Just <$> str

data HeadMoveDirection = DLeft | DRight

instance Show HeadMoveDirection where
  show DLeft = "L"
  show DRight = "R"

moveHead :: Tape -> HeadMoveDirection -> Tape
(Tape (m':left') m right) `moveHead` DLeft = Tape left' m' (m:right)
(Tape left m (m':right')) `moveHead` DRight = Tape (m:left) m' right'

write :: Tape -> Maybe Char -> Tape
(Tape left _ right) `write` char = Tape left char right

type State = Int

type TMConfig = (State, Tape)

data TMRule = TMRule
  { state :: State
  , char :: Maybe Char
  , nextState :: State
  , writeChar :: Maybe Char
  , direction :: HeadMoveDirection
  }

instance Show TMRule where
  show (TMRule state char nextState writeC direction)
    = [i|#{state} --#{char'}/#{writeC'};#{direction}--> #{nextState}|]
    where char' = toChar char
          writeC' = toChar writeC
          toChar char = case char of Just c -> [c]; Nothing -> "_"

appliesTo :: TMRule -> TMConfig -> Bool
(TMRule state char _ _ _) `appliesTo` (state', Tape _ char' _)
  = state == state' && char == char'

follow :: TMRule -> TMConfig -> TMConfig
(TMRule _ _ nextS writeC direction) `follow` (_, tape) = (nextS, tape')
  where tape' = tape `write` writeC `moveHead` direction

type DTMRulebook = [TMRule]

nextConfig :: DTMRulebook -> TMConfig -> TMConfig
rules `nextConfig` config = rules `ruleFor` config `follow` config
  where ruleFor :: DTMRulebook -> TMConfig -> TMRule
        rules `ruleFor` config = case filter (`appliesTo` config) rules of
          [rule] -> rule
          _ -> TMRule (-1) Nothing (-1) Nothing DLeft

data DTM = DTM
  { currentConfig :: TMConfig
  , acceptStates :: [State]
  , rulebook :: DTMRulebook
  }

accepting :: DTM -> Bool
accepting (DTM (state, _) acceptStates _) = state `elem` acceptStates

step :: DTM -> DTM
step dtm@(DTM config _ rules) = dtm { currentConfig = rules `nextConfig` config }

run :: DTM -> DTM
run dtm = if accepting dtm || stuck then dtm else run $ step dtm
  where stuck = case dtm of (DTM (-1, _) _ _) -> True; _ -> False
