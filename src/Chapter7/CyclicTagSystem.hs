module Chapter7.CyclicTagSystem where

import Data.List

import qualified Chapter7.TagSystem as T

data CyclicChar = Zero | One deriving (Show, Eq)

type CyclicString = [CyclicChar]

newtype CyclicTagRule = CyclicTagRule CyclicString deriving (Show, Eq)
type CyclicTagRulebook = [CyclicTagRule]

data CyclicTagSystem = CyclicTagSystem
  { currentString :: CyclicString
  , rules :: CyclicTagRulebook
  }

appliesTo :: CyclicString -> Bool
appliesTo (One:_) = True
appliesTo _ = False

nextString :: CyclicTagRulebook -> CyclicString -> (CyclicTagRulebook, CyclicString)
rules `nextString` str = tail <$> rules `followNextRule` str

followNextRule :: CyclicTagRulebook -> CyclicString -> (CyclicTagRulebook, CyclicString)
(rule:rest) `followNextRule` str = (rules', str')
  where rules' = rest ++ [rule]
        str' = if appliesTo str then rule `follow` str else str
        follow :: CyclicTagRule -> CyclicString -> CyclicString
        (CyclicTagRule appendCharacters) `follow` str = str ++ appendCharacters

step :: CyclicTagSystem -> CyclicTagSystem
step self@(CyclicTagSystem str rules) = CyclicTagSystem str' rules'
  where (rules', str') = rules `nextString` str

toRule :: String -> CyclicTagRule
toRule = CyclicTagRule . fmap toCyclicChar

toCyclicChar char = case char of '0' -> Zero; '1' -> One

alphabet :: T.TagSystem -> String
alphabet (T.TagSystem str (T.TagRulebook _ rules)) = sort . nub $ str ++ ruleChars
  where ruleChars = rules >>= extract
        extract (T.TagRule char str) = char:str

newtype CyclicTagEncoder = CyclicTagEncoder String
calcEncoder :: T.TagSystem -> CyclicTagEncoder
calcEncoder = CyclicTagEncoder . alphabet

encodeString :: CyclicTagEncoder -> String -> CyclicString
encoder `encodeString` str = str >>= encodeCharacter encoder

encodeCharacter :: CyclicTagEncoder -> Char -> CyclicString
(CyclicTagEncoder alphabet) `encodeCharacter` char = map encode [0 .. length alphabet - 1]
  where (Just characterPosition) = elemIndex char alphabet
        encode n = if n == characterPosition then One else Zero

cyclicRules :: T.TagRulebook -> CyclicTagEncoder -> CyclicTagRulebook
rules `cyclicRules` self@(CyclicTagEncoder alphabet) = map cyclicRuleFor alphabet
  where cyclicRuleFor char = CyclicTagRule $ case rules `T.ruleFor` [char] of
          Nothing -> []
          Just (T.TagRule _ appendCharacters) -> self `encodeString` appendCharacters

cyclicPaddingRules :: T.TagRulebook -> CyclicTagEncoder -> CyclicTagRulebook
(T.TagRulebook deletionNumber _) `cyclicPaddingRules` self@(CyclicTagEncoder alphabet) = replicate length' emptyRule
  where emptyRule = toRule ""
        length' = length alphabet * (deletionNumber - 1)

toCyclic :: T.TagSystem -> CyclicTagSystem
toCyclic system@(T.TagSystem str rules) = CyclicTagSystem encodedStr encodedRules
  where encoder = calcEncoder system
        encodedStr = encoder `encodeString` str
        encodedRules = concat $ (\f -> f rules encoder) <$> [cyclicRules, cyclicPaddingRules]
