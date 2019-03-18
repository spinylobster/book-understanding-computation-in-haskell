module Chapter4.Analyse
  ( analyse
  , syntaxAnalyzer
  ) where

import Chapter3.Automaton
import Chapter4.DPDA (pdaRule)
import Chapter4.NPDA

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Char
import Text.Regex.PCRE

type Token = Char
type Pattern = String
type Rule = (Token, Pattern)
type Grammer = [Rule]
type MatchData = (Bool, String)

grammer :: Grammer
grammer = [
    ('i', "if")
  , ('e', "else")
  , ('w', "while")
  , ('d', "do-nothing")
  , ('(', "\\(")
  , (')', "\\)")
  , ('{', "{")
  , ('}', "}")
  , (';', ";")
  , ('=', "=")
  , ('+', "\\+")
  , ('*', "\\*")
  , ('<', "<")
  , ('n', "[0-9]+")
  , ('b', "true|false")
  , ('v', "[a-z]+")
  ]

analyse :: String -> [Token]
analyse str = case ruleMatching str of
  Nothing -> []
  Just ((token, _), (_, matchStr))
    -> let rest = dropWhile isSpace $ drop (length matchStr) str
    in token : analyse rest

ruleMatching :: String -> Maybe (Rule, MatchData)
ruleMatching str
  = fmap ((`matchAtBeginning` str) . snd) grammer
  & zip grammer
  & filter (fst . snd)
  & listToMaybe . sortOn (Down . (snd . snd))

matchAtBeginning :: Pattern -> String -> MatchData
matchAtBeginning pat str = (match regex str, match regex str)
  where regex = makeRegexOpts compFirstLine execBlank pat'
        pat' = '^':pat

syntaxAnalyzer :: NPDA
syntaxAnalyzer = makeNpda [3] rulebook
  where
    rulebook = [startRule, stopRule] <> symbolRules <> tokenRules
    startRule = pdaRule (1, '$', Nothing) ("S$", 2)
    stopRule = pdaRule (2, '$', Nothing) ("$", 3)
    symbolRules = (\(c, s) -> pdaRule (2, c, Nothing) (s, 2)) <$>
      [ ('S', "W"), ('S', "A")
      , ('W', "w(E){S}")
      , ('A', "v=E")
      , ('E', "L")
      , ('L', "M<L"), ('L', "M")
      , ('M', "T*M"), ('M', "T")
      , ('T', "n"), ('T', "v")
      ]
    tokenRules = (\t -> pdaRule (2, t, Just t) ("", 2)) . fst <$> grammer
