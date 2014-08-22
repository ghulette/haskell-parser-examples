module Expr where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)

data Expr = Abs Id Expr
          | App Expr Expr
          | Var Id
          | Num Int
          | Binop Op Expr Expr
          deriving (Eq,Show)

source :: Expr -> String
source (Abs x e) = "(\\" ++ x ++ " -> " ++ source e ++ ")"
source (App e1 e2) = "(" ++ source e1 ++ ") (" ++ source e2 ++ ")"
source (Var x) = x
source (Num n) = show n
source (Binop op e1 e2) = "(" ++ source e1 ++ sourceOp op ++ source e2 ++ ")"
  where sourceOp Add = " + "
        sourceOp Sub = " - "
        sourceOp Mul = " * "

addExpr :: Expr -> Expr -> Expr
addExpr = Binop Add

subExpr :: Expr -> Expr -> Expr
subExpr = Binop Sub

mulExpr :: Expr -> Expr -> Expr
mulExpr = Binop Mul
