module Lib
    ( eval
    , Exp(..)
    , Val(..)
    , liftIntOp
    ) where

data Val = IntVal Integer
         | ClosureVal [String] Exp Env
   deriving (Show,Eq)

data Exp = IntExp Integer
         | IntOpExp String Exp Exp
         | VarExp String
         | LetExp String Exp Exp
         | FunExp [String] Exp
         | AppExp Exp [Exp]
   deriving (Show,Eq)

type Env = [(String,Val)]

intOps = [("+",(+))
         ,("-",(-))
         ,("*",(*))
         ,("ssqq",sumSqr)
         ,("/",div)]

sumSqr a b = a ^ 2 + b ^ 2

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _           _           = IntVal 0

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i


-- 5 + 10

-- =>   IntOpExp "+" (IntOpExp "*" (IntExp 103) (IntExp 5)) (IntExp 10)
--                op -------------------------------------- -----------
--                    e1 -- eval --> (IntVal 515)             e2

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env -- (IntVal 515)
      v2 = eval e2 env -- (IntVal 10)
      Just f = lookup op intOps -- (+)
   in liftIntOp f v1 v2

eval (VarExp v) env =
  case lookup v env of
    Just vv -> vv
    Nothing -> IntVal 0

eval (LetExp v e1 e2) env =
  let v1 = eval e1 env
  in eval e2 ((v,v1) : env)


eval (FunExp v e1) env =
  ClosureVal v e1 env

eval (AppExp e1 e2) env =
  let ClosureVal v body clenv = eval e1 env
      v2 = map (\vv->eval vv env) e2
   in eval body ((zip v v2) ++ clenv)


-- p1 = LetExp "inc"
--        (LetExp "i" (IntExp 1)
--           (FunExp "x" (IntOpExp "+" (VarExp "x") (VarExp "i"))))
--        (AppExp (VarExp "inc") (IntExp 41))
