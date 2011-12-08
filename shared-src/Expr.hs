module Expr (Expr(..),Binop(..),run) where

import Env

type Id = String

data Binop = Add | Sub | Mul deriving (Show)

data Expr = Abs Id Expr
          | Clo Id Expr (Env Expr)
          | App Expr Expr
          | Var Id
          | Num Int
          | Op Binop Expr Expr
          deriving (Show)

binop :: Binop -> (Int -> Int -> Int)
binop Add = (+)
binop Sub = (-)
binop Mul = (*)

evalNum :: Expr -> Env Expr -> Int
evalNum e env = 
  case eval e env of
    Num n -> n
    _ -> undefined

eval :: Expr -> Env Expr -> Expr
eval (Num n) _ = Num n
eval (Op op e1 e2) env =
  let n1 = evalNum e1 env
      n2 = evalNum e2 env
      opf = binop op
  in Num (opf n1 n2)
eval (Abs x e) env = Clo x e env
eval (Clo _ _ _) _ = undefined
eval (Var x) env = 
  case apply x env of
    Just v -> v
    Nothing -> undefined
eval (App e1 e2) env = 
  let v = eval e2 env in
  case eval e1 env of
    Clo x e1' env' -> eval e1' (extend env' x v)
    _ -> undefined

run :: Expr -> Expr
run e = eval e empty
