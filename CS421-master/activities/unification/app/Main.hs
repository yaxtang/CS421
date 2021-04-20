module Main where

import Lib

import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import Data.List (intersperse)

data Entity = Var String
            | Object String [Entity]
  deriving (Eq)

-- alpha ==> Var "alpha"
-- f(alpha)  ==> Object "f" [Var "alpha"]
-- f(x,beta)  ==> Object "f" [Object "x" [], Var "beta"]

instance Show Entity where
  show (Var s) = s
  show (Object s []) = s
  show (Object f xx) = concat $ f : "(" : intersperse "," (map show xx) ++ [")"]

isVar (Var _) = True
isVar _ = False

-- Environment functions

type Env = H.HashMap String Entity

initial :: Env
initial = H.empty

add :: String -> Entity -> Env -> Env
add x y b = H.insert x y b

contains :: String -> Env -> Bool
contains x b = H.member x b

lookup :: String -> Env -> Entity
lookup x b = fromJust (H.lookup x b)

-- Functions you get to write

phi :: Env -> Entity -> Entity
phi env (Var s) = lookup s env
phi env (Object s xx) = Object s (map (phi env) xx)

liftit f (s,t) = (f s, f t)

occurs :: String -> Entity -> Boolean
occurs s (Var x) = s == x
occurs s (Object _ xx) = any (occurs s) xx

unify :: [(Entity,Entity)] ->
unify [] = initial
unify ((s,t):c') = undefined
  let env = unify c' in
      liftit (phi env) (s,t) 
