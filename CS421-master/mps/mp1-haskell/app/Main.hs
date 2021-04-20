--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Main where

main :: IO ()
main = return ()

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int->[a]->[a]
mytake _ [] = []
mytake n (x:xs) = if n>0 then x:(mytake (n-1) xs) else []

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int->[a]->[a]
mydrop _ [] = []
mydrop n (x:xs) = if n>0 then mydrop (n-1) xs else (x:xs)

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a]->[a]
rev l = helper l []
        where helper [] acc= acc
              helper (x:xs) acc = helper xs (x:acc) 

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a]->[a]->[a]
app xs [] = xs
app [] ys = ys
app (x:xs) ys = x: (app xs ys)

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a]->[a]
inclist [] = []
inclist (x:xs) = (x+1):inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a]->a
sumlist l = helper l 0
            where helper [] acc= acc
                  helper (x:xs) acc= helper xs (acc+x)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip:: [a]->[b]->[(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y):(myzip xs ys)

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xs ys = helper (myzip xs ys)
               where helper ((x,y):zs) = (x+y): (helper zs)
                     helper [] = []

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1:ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = helper 0
       where helper x = x:(helper (x+1))

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0:1: (addpairs fib (tail fib))

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] ->[a]
add n [] = n:[]
add n (x:xs)
    | n<x = n:x:xs
    | n == x = x:xs
    | otherwise = x:(add n xs)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union n m = helper n m []
              where helper [] ys acc = (reverse acc)++ys
                    helper xs [] acc = (reverse acc)++xs
                    helper (x:xs) (y:ys) acc
                        | x<y = helper xs (y:ys) (x:acc)
                    	| x>y = helper (x:xs) ys (y:acc)
                    	| otherwise = helper xs ys (x:acc)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
    | (x<y) = intersect xs (y:ys)
    | (x==y) = x:(intersect xs ys)
    | otherwise = intersect (x:xs) ys
--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (map (add x) (powerset xs)) (powerset xs)

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = map (+1) 

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = foldr (+) 0

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a]-> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs) 

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x l) = x:(cons2list l)

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x)= x
eval (PlusExp []) = 0
eval (PlusExp (e:es)) = eval e + (eval (PlusExp es))
eval (MultExp []) = 1
eval (MultExp (e:es)) = eval e * (eval (MultExp es))

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' l = foldr (\x-> \acc-> (Cons x acc)) Nil l

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf 
               deriving Show

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node x l r)= x + (sumTree l)+(sumTree r)

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer 
             | BoolVal Bool 
             | StrVal String 
             | ExnVal String 
             deriving Show

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp:: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal (x `op` y)
liftIntOp _ _ _ = ExnVal "not an IntVal!"
