module Lib
    ( Calc(..)
    , calc
    ) where

data Calc a = Add a
            | Sub a
   deriving (Eq,Show)


calc :: Num a => [Calc a] -> a -> (a -> a) -> (a -> a) -> a
calc xx init ka ks = aux init xx ka ks
  where aux a [] ka ks =  ks (ka a)
        aux a ((Add i):xs) ka ks = aux a xs (\v-> ka $ i+v) ks
        aux a ((Sub i):xs) ka ks = aux a xs ka (\v-> ks $ v-i)