module ParsecParser (parseExpr) where

import Expr
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token


-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
          Token.reservedOpNames = ["->","\\","+","*","-","="],
          Token.reservedNames = ["let","in","end"],
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
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- natural
  return (Num (fromIntegral n))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return (Abs x e)

letin :: Parser Expr
letin = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  reserved "end"
  return (App (Abs x e2) e1)

termOp :: Parser (Expr -> Expr -> Expr)
termOp = (reservedOp "+" >> return addExpr) <|>
         (reservedOp "-" >> return subExpr) <?>
         "term operator"

factorOp :: Parser (Expr -> Expr -> Expr)
factorOp = (reservedOp "*" >> return mulExpr) <?>
           "factor operator"

expr :: Parser Expr
expr = fmap (foldl1 App) (many1 rdx)
  where rdx = term `chainl1` termOp
        term = factor `chainl1` factorOp
        factor = parens expr <|>
                 number <|>
                 variable <|>
                 lambda <|>
                 letin <?>
                 "expression"

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
