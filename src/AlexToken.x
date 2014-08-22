{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..),scanTokens) where
import Expr
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
  $digit+                       { \s -> TokenNum (read s) }
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
           | TokenLambda
           | TokenNum Int
           | TokenSym String
           | TokenArrow
           | TokenEq
           | TokenOp Op
           | TokenLParen
           | TokenRParen
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
