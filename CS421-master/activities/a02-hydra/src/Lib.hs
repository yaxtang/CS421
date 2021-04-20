module Lib
    ( someFunc
    , chop
    ) where

-- Your code here!
-- Feel free to write helper functions.

-- chop :: [Int] -> [Int]

chop :: [Int] -> [Int]
chop x  = helper x [] (length x) 0
  where helper [] a _ _=reverse a
        helper (x:xs) a l flag | (x==0 && flag==0) = helper xs (x:a) (l-1) 0
                               | (x/=0 && flag==0) = helper xs (x-1:a) (l-1) 1
                               | flag==1 = helper xs ((x+l):a) (l-1) 2
                               | otherwise = helper xs (x:a) (l-1) 2

someFunc :: IO ()
someFunc = putStrLn "someFunc"
