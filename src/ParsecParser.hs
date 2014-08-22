module ParsecParser (parseExpr) where

import Expr
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token


-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedOpNames = ["->","\\","+","*","-","="],
          Token.reservedNames = ["let","in"],
          Token.commentLine = "#" }

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

identifier :: Parser String
identifier = Token.identifier lexer


-- Parser

variable :: Parser Expr
variable = Var `fmap` identifier

number :: Parser Expr
number = (Num . fromIntegral) `fmap` natural

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return (App (Abs x e2) e1)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return (Abs x e)

expr :: Parser Expr
expr = letin <|> lambda <|> formula

formula :: Parser Expr
formula = buildExpressionParser [[mulOp],[addOp,subOp]] juxta <?> "formula"
  where addOp = Infix (reservedOp "+" >> return addExpr) AssocLeft
        subOp = Infix (reservedOp "-" >> return subExpr) AssocLeft
        mulOp = Infix (reservedOp "*" >> return mulExpr) AssocLeft

juxta :: Parser Expr
juxta = (foldl1 App) `fmap` (many1 atom)

atom :: Parser Expr
atom = variable <|> number <|> parens expr <?> "atom"

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Expr
parseExpr t = 
  case parse (allOf expr) "stdin" t of
    Left err -> error (show err)
    Right ast -> ast
