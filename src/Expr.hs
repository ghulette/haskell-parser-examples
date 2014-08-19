module Expr where

type Id = String

data Op = Add | Sub | Mul deriving (Eq,Show)

data Expr = Abs Id Expr
          | App Expr Expr
          | Var Id
          | Num Int
          | Binop Op Expr Expr
          deriving (Eq,Show)

