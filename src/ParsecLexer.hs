module ParsecLexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellStyle)

-- Tokens

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

allOf :: Parser a -> Parser a
allOf p = do
  Token.whiteSpace lexer
  r <- p
  eof
  return r
