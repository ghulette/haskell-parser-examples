module Env (Env,empty,extend,apply) where

-- Environment

type Env a = [(String,a)]

empty :: Env a
empty = []

extend :: Env a -> String -> a -> Env a
extend env x v = (x,v):env

apply :: String -> Env a -> Maybe a
apply = lookup
