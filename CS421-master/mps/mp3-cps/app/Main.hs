--- Given Code
--- ==========

module Main where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

fromParse :: Either ParseError Exp -> Exp
fromParse (Right exp) = exp
fromParse (Left err)  = error $ show err

ctorParse :: String -> String
ctorParse = ctorShow . fromParse . parseExp

--- The Parser
--- ----------

-- Pretty parser type
type Parser = ParsecT String () Identity

--- ### Lexicals

symbol :: String -> Parser String
symbol s = do string s
              spaces
              return s

int :: Parser Integer
int = do digits <- many1 digit <?> "an integer"
         spaces
         return (read digits :: Integer)

var :: Parser String
var = let keywords = ["if", "then", "else"]
      in  try $ do v1 <- letter                  <?> "an identifier"
                   vs <- many (letter <|> digit) <?> "an identifier"
                   spaces
                   let v = v1:vs
                   if (any (== v) keywords)
                    then fail "keyword"
                    else return v

oper :: Parser String
oper = do op <- many1 (oneOf "+-*/<>=") <?> "an operator"
          spaces
          return op

parens :: Parser a -> Parser a
parens p = do symbol "("
              pp <- p
              symbol ")"
              return pp

--- ### Expressions

intExp :: Parser Exp
intExp = do i <- int
            return $ IntExp i

varExp :: Parser Exp
varExp = do v <- var
            return $ VarExp v

opExp :: String -> Parser (Exp -> Exp -> Exp)
opExp str = do symbol str
               return (OpExp str)

mulOp :: Parser (Exp -> Exp -> Exp)
mulOp = opExp "*" <|> opExp "/"

addOp :: Parser (Exp -> Exp -> Exp)
addOp = opExp "+" <|> opExp "-"

compOp :: Parser (Exp -> Exp -> Exp)
compOp = try (opExp "<=") <|> try (opExp ">=")
         <|> opExp "<"    <|> opExp ">"
         <|> opExp "/="   <|> opExp "=="

ifExp :: Parser Exp
ifExp = do try $ symbol "if"
           e1 <- expr
           symbol "then"
           e2 <- expr
           symbol "else"
           e3 <- expr
           return $ IfExp e1 e2 e3

lamExp :: Parser Exp
lamExp = do try $ symbol "\\"
            param <- var
            symbol "->"
            body <- expr
            return $ LamExp param body

atom :: Parser Exp
atom =     intExp
       <|> ifExp
       <|> lamExp
       <|> varExp
       <|> parens expr

expr :: Parser Exp
expr = let arith  = term `chainl1` addOp
           term   = factor `chainl1` mulOp
           factor = app
           app    = do f <- many1 atom
                       return $ foldl1 AppExp f
       in  arith `chainl1` compOp

parseExp :: String -> Either ParseError Exp
parseExp str = parse expr "stdin" str

--- ### Declarations

decl :: Parser Stmt
decl = do f <- var
          params <- many1 var
          symbol "="
          body <- expr
          return $ Decl f params body

parseDecl :: String -> Either ParseError Stmt
parseDecl str = parse decl "stdin" str

--- The REPL
--- --------

prompt :: String -> IO ()
prompt str = hPutStr stdout str >> hFlush stdout

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: IO ()
repl = do input <- prompt "> " >> getLine
          case input of
            "quit" -> return ()
            _      -> do case parseDecl input of
                            Left err    -> do printLn "Parse error!"
                                              printLn $ show err
                            Right decl  -> printLn . show $ cpsDecl decl
                         repl


main :: IO ()
main = do putStrLn "Welcome to the CPS Transformer!"
          repl
          putStrLn "GoodBye!"


--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk x k = factk (x-1) (\v -> k (x*v))


--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x:[]) k1 k2 = if (x `rem` 2 == 0) then k1 x else k2 x
evenoddk (x:xs) k1 k2 = if (x `rem` 2 == 0) then evenoddk xs (\v-> k1 (x+v)) k2 else evenoddk xs k1 (\v-> k2 (x+v))
 
--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (AppExp _ _)= False
isSimple (IfExp e1 e2 e3) = (isSimple e1) && (isSimple e2) && (isSimple e3)
isSimple (OpExp s e1 e2) = (isSimple e1) && (isSimple e2)
isSimple _ = True 

--- ### Define `cpsExp` - Overview
--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)
cpsExp (IntExp i) k num = (AppExp k (IntExp i),num) 
cpsExp (VarExp v) k num = (AppExp k (VarExp v),num)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k num = if isSimple e then (AppExp (AppExp f e) k,num) 
                                          else let (v,n) = gensym num in cpsExp e (LamExp v (AppExp (AppExp f (VarExp v)) k)) n

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k num
                        | (isSimple e1) && (isSimple e2) = (AppExp k (OpExp op e1 e2),num)
                        | isSimple e2 = let (v,n) = gensym num in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) n
                        | isSimple e1 = let (v,n) = gensym num in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) n
                        | otherwise =  cpsExp e1 (LamExp v1 a) n3
                                               where (v1,n1) = gensym num
                                                     (v2,n2) = gensym n1
                                                     (a,n3) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) n2
      
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k num
                        | (isSimple e1) = let (t1,_) = cpsExp e2 k num
                                              (t2,_) = cpsExp e3 k num 
                                          in (IfExp e1 t1 t2,num) 
                                        
                        | otherwise =  cpsExp e1 (LamExp v (IfExp (VarExp v) t1 t2)) n
                                               where (v,n) = gensym num
                                                     (t1,_) = cpsExp e2 k n
                                                     (t2,_) = cpsExp e3 k n 


--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl s ss e) = Decl s (ss++["k"]) k1 
                          where (k1,_) = cpsExp e (VarExp "k") 0
