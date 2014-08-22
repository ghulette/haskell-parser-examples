{
module Parser where

import Token
import Expr
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    let  { TokenLet }
    in   { TokenIn }
    int  { TokenInt $$ }
    var  { TokenSym $$ }
    '\\' { TokenLambda }
    '->' { TokenArrow }
    '='  { TokenEq }
    '+'  { TokenOp $$ }
    '-'  { TokenOp $$ }
    '*'  { TokenOp $$ }
    '('  { TokenLParen }
    ')'  { TokenRParen }

%%

Expr : L1 { $1 }

L1 : let var '=' Expr in L1      { App (Abs $2 $6) $4 }
   | L2                          { $1 }

L2 : '\\' var '->' L2            { Abs $2 $4 }
   | L3                          { $1 }

L3 : L3 L4                       { App $1 $2 }
   | L4                          { $1 }

L4 : L4 '+' L5                   { Binop $2 $1 $3 }
   | L4 '-' L5                   { Binop $2 $1 $3 }
   | L5                          { $1 }

L5 : L5 '*' L6                   { Binop $2 $1 $3 }
   | L6                          { $1 }

L6 : '(' Expr ')'                { $2 }
   | int                         { Num $1 }
   | var                         { Var $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens

}
