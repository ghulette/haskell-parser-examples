module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import Lexer
import Expr

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

factor :: Parser Expr
factor =  parens expr
      <|> variable
      <|> number
      <|> lambda
      <|> letin
      <?> "factor"

term :: Parser Expr
term = Ex.buildExpressionParser table factor
  where infixOp x f = Ex.Infix (reservedOp x >> return f)
        table = [[infixOp "*" (Op Mul) Ex.AssocLeft],
                 [infixOp "+" (Op Add) Ex.AssocLeft,
                  infixOp "-" (Op Sub) Ex.AssocLeft]]

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)

parseExpr :: String -> Expr
parseExpr t = 
  case parse (allOf expr) "" t of
    Left err -> error (show err)
    Right ast -> ast
