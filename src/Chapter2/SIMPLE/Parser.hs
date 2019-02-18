module Chapter2.SIMPLE.Parser
  ( parseSimple
  ) where

import Chapter2.SIMPLE.Expression
import Chapter2.SIMPLE.Statement
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

parseSimple :: String -> Either ParseError Statement
parseSimple = parse simpleParser ""
simpleParser = toSequence <$> stats
  where toSequence = foldl Sequence DoNothing
        stats = sepBy stmt $ const DoNothing <$> (many $ oneOf ";\n\r ")

boolean = choice (try <$> [true, false]) <?> "boolean"
  where
    makeBoolTerm :: Bool -> String -> Parser Expression
    makeBoolTerm b str = const (Boolean b) <$> do { string str; space }
    true = makeBoolTerm True "true"
    false = makeBoolTerm False "false"

variable = (Variable <$> P.identifier lexer) <?> "variable"

singleSpace = char ' '

assign = do
  Variable name <- variable
  many singleSpace; char '='; many singleSpace
  Assign name <$> expr

doNothing = DoNothing <$ string "do-nothing"

if' = do
  string "if ("; many singleSpace
  cond <- expr
  string ") {"; spaces
  cons <- stmt
  spaces; string "} else {"; spaces
  alt <- stmt
  spaces; char '}'
  return $ If cond cons alt

while = do
  string "while ("; many singleSpace
  cond <- expr
  string ") {"; spaces
  body <- stmt
  spaces; char '}'
  return $ While cond body

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { P.reservedOpNames = ["&&", "||", "<", "*", "+"] })

natural = Number <$> P.natural lexer
parens = P.parens lexer
reservedOp = P.reservedOp lexer

expr :: Parser Expression
expr = buildExpressionParser table term <?> "expression"
  where
    table = [[binop "&&" And AssocLeft, binop "||" Or AssocLeft,
              binop "<" LessThan AssocLeft,
              binop "*" Multiply AssocLeft, binop "+" Add AssocLeft]]
    binop s op = Infix (do{ reservedOp s; return op } <?> "operator")
    unary s op = Prefix (do{ reservedOp s; return op })

term :: Parser Expression
term = choice (try <$> factors) <?> "term"
  where factors = [variable, parens expr, natural, boolean]

stmt :: Parser Statement
stmt = choice (try <$> [assign, doNothing, if', while]) <?> "statement"
