{
module HappyParser where

import AlexToken
import Expr
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    let   { TokenLet }
    in    { TokenIn }
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '\\'  { TokenLambda }
    '->'  { TokenArrow }
    '='   { TokenEq }
    '+'   { TokenOp $$ }
    '-'   { TokenOp $$ }
    '*'   { TokenOp $$ }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%%

Expr : let VAR '=' Expr in Expr    { App (Abs $2 $6) $4 }
     | '\\' VAR '->' Expr          { Abs $2 $4 }
     | Exp1                        { $1 }

Exp1 : Exp1 '+' Term               { Binop $2 $1 $3 }
     | Exp1 '-' Term               { Binop $2 $1 $3 }
     | Term                        { $1 }

Term : Term '*' Juxt               { Binop $2 $1 $3 }
     | Juxt                        { $1 }

Juxt : Juxt Atom                   { App $1 $2 }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Num $1 }
     | VAR                         { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}
