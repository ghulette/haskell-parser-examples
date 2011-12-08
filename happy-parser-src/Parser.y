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
    end  { TokenEnd }
    int  { TokenInt $$ }
    var  { TokenSym $$ }
    '\\' { TokenLambda }
    '->' { TokenArrow }
    '='  { TokenEq }
    op   { TokenOp $$ }
    '('  { TokenLParen }
    ')'  { TokenRParen }

%left '+' '-'
%left '*' '/'

%%

Exprs : Expr                             { $1 }
      | Exprs Expr                       { App $1 $2 }

Expr : Exprs                             { $1 }
     | let var '=' Expr in Expr end      { App (Abs $2 $6) $4 }
     | '\\' var '->' Expr                { Abs $2 $4 }
     | Expr op Expr                      { Op (opEnc $2) $1 $3 }
     | '(' Expr ')'                      { $2 }
     | int                               { Num $1 }
     | var                               { Var $1 }

{

opEnc :: String -> Binop
opEnc "+" = Add
opEnc "-" = Sub
opEnc "*" = Mul

parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens

}
