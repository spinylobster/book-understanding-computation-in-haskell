module Chapter7.TagSystem where

import Data.Maybe

data TagRule = TagRule Char String
data TagRulebook = TagRulebook Int [TagRule]
data TagSystem = TagSystem
  { currentString :: String
  , rules :: TagRulebook
  }

class Appliable a where
  appliesTo :: a -> String -> Bool

instance Appliable TagRule where
  (TagRule firstChar _) `appliesTo` [] = False
  (TagRule firstChar _) `appliesTo` (x:_) = firstChar == x

follow :: TagRule -> String -> String
(TagRule _ appendCharacters) `follow` string = string ++ appendCharacters

nextString :: TagRulebook -> String -> String
self@(TagRulebook deletionNumber rules) `nextString` string
  = drop deletionNumber . (`follow` string) $ rule
  where (Just rule) = self `ruleFor` string

ruleFor :: TagRulebook -> String -> Maybe TagRule
(TagRulebook _ rules) `ruleFor` string = listToMaybe . filter (`appliesTo` string) $ rules

step :: TagSystem -> TagSystem
step self@(TagSystem currentString rulebook) = self { currentString = rulebook `nextString` currentString }

instance Appliable TagRulebook where
  self@(TagRulebook deletionNumber _) `appliesTo` string = ruleExists && stringLengthIsOk
    where ruleExists = isJust $ self `ruleFor` string
          stringLengthIsOk = length string >= deletionNumber

run :: TagSystem -> String
run self@(TagSystem currentString rulebook)
  = if rulebook `appliesTo` currentString
    then run (step self)
    else currentString
