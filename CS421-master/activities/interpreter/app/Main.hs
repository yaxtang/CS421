module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ eval (IntOpExp "+" (IntExp 20) (IntExp 22)) []
