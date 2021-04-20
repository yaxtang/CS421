{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Scheme.Eval where

import Scheme.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

-- ### Evaluation helpers

-- Evaluates a symbol to string
-- Throws an error if value is not a symbol
-- Examples:
--   getSym (Symbol "x")  ==> "x"
--   getSym (Number 1)    ==> Not a symbol: x
getSym :: Val -> EvalState String
getSym (Symbol x) = return x
getSym         v  = throwError $ NotASymbol v

-- `let` and `let*`
getBinding :: Val -> EvalState (String, Val)
getBinding (List [c, e]) = liftM2 (,) (getSym c) (eval e)
getBinding v = throwError $ NotAListOfTwo v

-- Evaluates a list of two to a tuple
-- Throws an error if value is not a list of two
-- This is useful in special form `cond`, since each clause
-- is expected to be exactly a two-element list
-- Examples:
--   getListOf2 (List (Number 1) (Symbol "x"))
--     ==> ((Number 1), (Symbol "x"))
--   getListOf2 (List (Number 1))
--     ==> Not a list of two elements: (1)
getListOf2 :: Val -> EvalState (Val, Val)
getListOf2 (List [c, e]) = return (c, e)
getListOf2 v = throwError $ NotAListOfTwo v

--- ### Keywords

-- When evaluating special forms, a list form starting with a keyword
-- is expected to match the special form syntax.
keywords :: [String]
keywords = [ "define"
           , "lambda"
           , "cond"
           , "let"
           , "let*"
           , "define-macro"
           , "quasiquote"
           , "unquote"
           ]

-- ### The monadic evaluator
-- Unlike evaluators in previous MPs, `eval` does not take any environment!
-- This is because the environment is encapsulated in the `EvalState` monad.
-- To access the environment, all you have to do is `get`, `modify` or `put`!
eval :: Val -> EvalState Val

-- Self-evaluating expressions
-- TODO: What's self-evaluating?
eval v@(Number _) = return v
eval v@(Boolean _) = return v

-- Symbol evaluates to the value bound to it
-- TODO
eval (Symbol sym) = do env <- get
                       case H.lookup sym env of              
                                Just v -> return v
                                _ -> throwError $ UndefSymbolError sym

-- Dotted list may just be an equivalent representation of List.
-- We simply try to flatten the list. If it's still dotted after
-- flattening, it's an invalid expression.
eval expr@(DottedList _ _) = case flattenList expr of
  DottedList _ _ -> throwError $ InvalidExpression expr
  v -> eval v

-- List evaluates as a form of the following
-- 1. Special form (`define`, `let`, `let*`, `cond`, `quote`, `quasiquote`,
--    `unquote`, `define-macro`, ...)
-- 2. Macro expansion (Macro)
-- 3. Function application (Func)
-- 4. Primitive function application (PrimFunc)
eval expr@(List lst) = evalList $ map flattenList lst where
    --- Evaluator for forms
    invalidSpecialForm :: String -> EvalState e
    invalidSpecialForm frm = throwError $ InvalidSpecialForm frm expr

    evalList :: [Val] -> EvalState Val

    evalList [] = throwError $ InvalidExpression expr

    -- quote
    -- TODO
    evalList [Symbol "quote", e] = return e
    -- unquote (illegal at surface evaluation)
    -- TODO: since surface-level `unquote` is illegal, all you need to do is
    -- to throw a diagnostic
    evalList [Symbol "unquote", e] = throwError $ UnquoteNotInQuasiquote e

    -- quasiquote
    evalList [Symbol "quasiquote", e] = evalQuasi 1 e where
      evalQuasi :: Int -> Val -> EvalState Val
      evalQuasi 0 (List [Symbol "unquote", v]) = throwError $ UnquoteNotInQuasiquote v
      evalQuasi 1 (List [Symbol "unquote", v]) = eval v
      evalQuasi n (List ee@[Symbol "quasiquote", _]) = List <$> evalQuasi (n+1) `mapM` ee
      evalQuasi n (List ee@[Symbol "unquote", _]) = List <$> evalQuasi (n-1) `mapM` ee
      evalQuasi n (List xx) = List <$> mapM (evalQuasi n) xx
      evalQuasi n (DottedList xx y) = DottedList <$> mapM (evalQuasi n) xx <*> evalQuasi n y
      evalQuasi _ v = return v

    -- Why comment these out? Because `if` can be defined as a macro!
    -- -- if-then
    -- evalList [Symbol "if", condE, thenE] =
    --   eval condE >>= \c -> if lowerBool c then eval thenE else return Void
    -- -- if-then-else
    -- evalList [Symbol "if", condE, thenE, elseE] =
    --   eval condE >>= \c -> eval $ if lowerBool c then thenE else elseE
    -- cond
    -- TODO: Handle `cond` here. Use pattern matching to match the syntax
    evalList ((Symbol "cond"):pairs) = case pairs of [] -> invalidSpecialForm "cond"
                                                     _ -> mapM getListOf2 pairs >>= evalCond 
                                                                  where evalCond [] = return Void
                                                                        evalCond [(Symbol "else",e)] = eval e
                                                                        evalCond ((Symbol "else",_):_) = invalidSpecialForm "cond"
                                                                        evalCond ((c,e):ces) = eval c >>= \cond-> case cond of Boolean False -> evalCond ces
                                                                                                                               _ -> eval e 

    -- let
    -- TODO: Handle `let` here. Use pattern matching to match the syntax
    evalList [(Symbol "let"),(List xes),body] = 
      do env <- get
         nxes <- mapM getBinding xes
         mapM_ (\(k,v)->modify (H.insert k v)) nxes
         val <- eval body
         put env 
         return val
    -- let*
    -- TODO: Handle `let*` here. Use pattern matching to match the syntax
    evalList [(Symbol "let*"),(List xes),body] = 
      do env <- get
         mapM_ (\xe -> (getBinding xe) >>= (\(k,v)->modify (H.insert k v))) xes
         val <- eval body
         put env 
         return val
         
         
    -- lambda
    -- TODO: Handle `lambda` here. Use pattern matching to match the syntax
    evalList [Symbol "lambda", List args, body] =
      do env <- get
         val <- (\argVal -> Func argVal body env) <$> mapM getSym args
         return val
    -- define function
    evalList [Symbol "define", List (Symbol fname : args), body] =
      do env <- get
         val <- (\argVal -> Func argVal body env) <$> mapM getSym args
         modify $ H.insert fname val
         return Void

    -- define variable
    -- TODO: Handle `define` for variables here. Use pattern matching
    -- to match the syntax
    evalList [Symbol "define", Symbol var , e] =
      do val <- eval e 
         modify $ H.insert var val
         return Void
    -- define-macro
    -- TODO: Handle `define-macro` here. Use pattern matching to match
    -- the syntax
    evalList [Symbol "define-macro",List ((Symbol fname):args),body] = 
      do val <- (\argVal -> Macro argVal body) <$> mapM getSym args
         modify $ H.insert fname val
         return Void
    -- invalid use of keyword, throw a diagnostic
    evalList (Symbol sym : _) | elem sym keywords = invalidSpecialForm sym

    -- application
    evalList (fexpr:args) = eval fexpr >>= aux where
      -- Macro expansion
      -- TODO: implement macro evaluation
      -- Use do-notation!
      aux (Macro fmls body) | length fmls == length args = 
        do env <- get
           mapM_ (\(k,v)-> modify (H.insert k v)) (zip fmls args)
           val1 <- eval body
           put env
           val2 <- eval val1
           return val2
      -- Function application
      -- TODO: evaluate arguments, and feed `f` along with the evaluated
      -- arguments to `apply`
      aux f = do nargs <- mapM eval args 
                 apply f nargs

eval val = throwError $ InvalidExpression val

-- Function application
apply :: Val -> [Val] -> EvalState Val
-- Function
-- TODO: implement function application
-- Use do-notation!
apply (Func fmls body cenv) args | length fmls == length args = 
  do env <- get
     modify (H.union cenv)
     mapM_ (\(k,v)-> modify (H.insert k v)) (zip fmls args)
     val <- eval body
     put env
     return val
-- Primitive
-- TODO: implement primitive function application
-- Since a primitive function has type `[Val] -> EvalState Val`, all you
-- need is to apply it to arguments
apply (PrimFunc p) args = p args
-- Other values are not applicable
-- TODO: you should simply throw a diagnostic
apply f args = throwError $ CannotApply f args
