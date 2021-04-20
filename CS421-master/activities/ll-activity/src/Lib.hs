module Lib
    ( Exp(..)
    , parse
    ) where

import Text.Regex.TDFA

isInt :: String -> Bool
isInt i = i =~ "[0-9]+"

isSymbol :: String -> String -> Bool
isSymbol s v = s == v

parseSymbol s (x:xs) =
  if isSymbol s x
     then (s,xs)
     else error $ "Parse error, expected " ++ s ++ " but got " ++ x ++ "."

-- Grammar
--
-- E -> + E E
--    | int
--    | var
--    | ( E )
--    | let var = E in E end

data Exp = PlusExp Exp Exp
         | IntExp Integer
         | VarExp String
         | LetExp String Exp Exp
    deriving (Show, Eq)

parse xx = parseE (words xx)

parseE (x:xs) | isSymbol "+" x =
  let (e1,r1) = parseE xs
      (e2,r2) = parseE r1
   in (PlusExp e1 e2, r2)

parseE (x:xs) | isInt x =
                (IntExp (read x), xs)



parseE (x:xs) | isSymbol "(" x = 
  let (e1,r1) = parseE xs
      (s2,r2) = parseSymbol ")" r1
   in (e1,r2)

parseE (x:xs) | isSymbol "let" x =
  let (VarExp s,r0) = parseE xs
      (s1,r1) = parseSymbol "=" r0
      (e1,r2) = parseE r1
      (s2,r3) = parseSymbol "in" r2
      (e2,r4) = parseE r3
      (s4,r5) = parseSymbol "end" r4
   in (LetExp s e1 e2,r5)

parseE (x:xs) | x =~ "[a-z]+"=
                (VarExp x, xs)


