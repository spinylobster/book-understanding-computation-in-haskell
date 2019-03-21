{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Chapter6.LambdaCalculus where

import Data.String.Interpolate
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P

data LCTerm = LCV String | LCF String LCTerm | LCC LCTerm LCTerm deriving (Eq)

instance Show LCTerm where
  show (LCV name) = name
  show (LCF param body) = [i|-> #{param} { #{body} }|]
  show (LCC left right) = [i|#{left}[#{right}]|]

replace :: LCTerm -> (String, LCTerm) -> LCTerm
self@(LCV name) `replace` (name', replacement) =
  if name == name' then replacement else self
self@(LCF param body) `replace` (name', replacement) =
  if param == name'
    then self
    else LCF param $ body `replace` (name', replacement)
self@(LCC left right) `replace` replacement =
  LCC (left `replace` replacement) (right `replace` replacement)

call :: LCTerm -> LCTerm -> LCTerm
(LCF param body) `call` arg = body `replace` (param, arg)

callable :: LCTerm -> Bool
callable (LCF _ _) = True
callable _ = False

reducible :: LCTerm -> Bool
reducible (LCC left right) = reducible left || reducible right || callable left
reducible _ = False

reduce :: LCTerm -> LCTerm
reduce (LCC left@(LCC _ _) right) = LCC (reduce left) right
reduce (LCC left right@(LCC _ _)) = LCC left (reduce right)
reduce (LCC left@(LCF _ _) right) = call left right

parseLambda :: String -> Either ParseError LCTerm
parseLambda = parse parseTerm ""
  where
    parseTerm = try calls <|> try variable <|> function
    calls = do
      first <- try variable <|> function
      let call = do
          char '['
          term <- parseTerm
          char ']'
          return term
      rest <- many1 call
      return $ foldl (\l r -> LCC l r) first rest
    variable = do
      name <- many1 alphaNum
      return $ LCV name
    function = do
      string "-> "
      name <- many1 alphaNum
      string " { "
      term <- parseTerm
      string " }"
      return $ LCF name term
