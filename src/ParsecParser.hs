module ParsecParser (parseExpr) where

import Expr

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellStyle)


-- Lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = haskellStyle { Token.reservedOpNames = ["->","\\","+","*","-","="],
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

expr :: Parser Expr
expr = do
  e1 <- expr2
  e2 <- expr2
  return (App e1 e2)

expr2 :: Parser Expr
expr2 = do
  e1 <- expr3
  op <- binop
  e2 <- expr3
  return (Binop op e1 e2)
  where binop = reservedOp "*" >> return Mul

expr3 :: Parser Expr
expr3 = do
  e1 <- expr4
  op <- binop
  e2 <- expr4
  return (Binop op e1 e2)
  where binop = (reservedOp "+" >> return Add) <|>
                (reservedOp "-" >> return Sub)

expr4 :: Parser Expr
expr4 = lambda <|> letin <|> number <|> variable <|> parens expr

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
