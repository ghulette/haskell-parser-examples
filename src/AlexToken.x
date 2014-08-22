{
{-# OPTIONS_GHC -w #-}
module Token (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn }
  end                           { \s -> TokenEnd }
  $digit+                       { \s -> TokenInt (read s) }
  "->"                          { \s -> TokenArrow }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]                          { \s -> TokenOp Add }
  [\-]                          { \s -> TokenOp Sub }
  [\*]                          { \s -> TokenOp Mul }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token = TokenLet
           | TokenIn
           | TokenEnd
           | TokenLambda
           | TokenInt Int
           | TokenSym String
           | TokenArrow
           | TokenEq
           | TokenTermOp String
           | TokenFactOp String
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
