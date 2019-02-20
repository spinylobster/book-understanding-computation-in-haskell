module Chapter3.Pattern
  ( Pattern(..)
  , inspect
  , toNfa
  , matches
  , parsePattern
  , ) where

import Chapter3.Automaton (State, accepts)
import Chapter3.NFA

import Data.List
import qualified Text.Parsec as P
import Text.Parsec.Char

data Pattern =
    Empty
  | Literal Char
  | Concatenate Pattern Pattern
  | Choose Pattern Pattern
  | Repeat Pattern
 deriving (Eq)

instance Show Pattern where
  show = show'
    where
      show' Empty = ""
      show' (Literal c) = [c]
      show' outer@(Concatenate first second) = concat $ bracket (precedence outer) <$> [first, second]
      show' outer@(Choose first second) = intercalate "|" $ bracket (precedence outer) <$> [first, second]
      show' outer@(Repeat pat) = bracket (precedence outer) pat ++ "*"
      precedence :: Pattern -> Int
      precedence Empty = 3
      precedence (Literal _) = 3
      precedence (Concatenate _ _) = 1
      precedence (Choose _ _) = 0
      precedence (Repeat _) = 2
      bracket :: Int -> Pattern -> String
      bracket outer_prec pat = if precedence pat < outer_prec
        then "(" ++ show pat ++ ")" else show pat

inspect :: Pattern -> String
inspect pat = "/" ++ show pat ++ "/"

toNfa :: Pattern -> NFA
toNfa pat = makeNfa [1] [as] rules
  where
    (as, rules) = toNfa' 1 pat
    toNfa' :: State -> Pattern -> (State, NFARulebook)
    toNfa' ss Empty = (ss, [])
    toNfa' ss (Literal c) = (as, [nfaRule (ss, Just c) as])
      where as = ss + 1
    toNfa' ss (Concatenate first second) = (as, rules)
      where
        (firstAs, firstRules) = toNfa' ss first
        (as, secondRules) = toNfa' firstAs second
        rules = firstRules ++ secondRules
    toNfa' ss (Choose first second) = (as, rules)
      where
        firstSs = ss + 1
        (firstAs, firstRules) = toNfa' firstSs first
        secondSs = firstAs + 1
        (secondAs, secondRules) = toNfa' secondSs second
        as = secondAs + 1
        freeMoves = uncurry freeMove <$> [(ss, firstSs), (ss, secondSs), (firstAs, as), (secondAs, as)]
        rules = firstRules ++ secondRules ++ freeMoves
    toNfa' ss (Repeat pat) = (as, rules)
      where
        (patAs, patRules) = toNfa' ss pat
        as = patAs + 1
        freeMoves = uncurry freeMove <$> [(ss, as), (patAs, ss)]
        rules = patRules ++ freeMoves
    freeMove from to = nfaRule (from, Nothing) to

matches :: Pattern -> String -> Bool
matches pat str = toNfa pat `accepts` str

parsePattern :: String -> Either P.ParseError Pattern
parsePattern = P.parse parser ""
  where literal = Literal <$> letter
        choose = do
          let pattern' = P.try brackets P.<|> P.try repeat P.<|> literal
          first <- P.try $ const Empty <$> char '|' P.<|> pattern'
          if first /= Empty then const () <$> char '|' else return ()
          second <- pattern
          return $ Choose first second
        repeat = do
          let pattern' = P.try brackets P.<|> literal
          pat <- pattern'
          char '*'
          return $ Repeat pat
        brackets = do
          char '('
          pat <- concatenate
          char ')'
          return pat
        concatenate = do
          pats <- P.many1 $ pattern
          return $ case pats of
            [p] -> p
            first:second:ps -> foldl Concatenate (Concatenate first second) ps
        pattern = P.try choose P.<|> P.try repeat P.<|> P.try brackets P.<|> literal
        parser = const Empty <$> P.eof P.<|> concatenate
