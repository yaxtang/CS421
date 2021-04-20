import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Testing Group 1" [
                testProperty "single var app" singleApp
              , testProperty "two variable app" twoApp
              , testProperty "four variable app" fourApp
           ]
      ]

singleLet v i =
    eval (LetExp v (IntExp i) (VarExp v)) [] == (IntVal $ i + 1)
  where types = (v :: String, i :: Integer)

singleApp v i =
    let app = FunExp [v] (IntOpExp "+" (VarExp v) (VarExp "_one"))
     in eval (AppExp app [IntExp i]) [("_one",IntVal 1)] == (IntVal $ i + 1)
  where types = (v :: String, i :: Integer)

twoApp v1 i1 v2 i2 =
    let vv1 = 'a':v1
        vv2 = 'b':v2
        app = FunExp [vv1, vv2] (IntOpExp "+" (VarExp vv1) (VarExp vv2))
     in eval (AppExp app [IntExp i1, IntExp i2]) [] == (IntVal $ i1 + i2)
  where types = (v1 :: String, v2 :: String , i1 :: Integer, i2 :: Integer)

fourApp v1 i1 v2 i2 v3 i3 v4 i4 =
    let vv1 = 'a':v1
        vv2 = 'b':v2
        vv3 = 'c':v3
        vv4 = 'd':v4
        app = FunExp [vv1, vv2, vv3, vv4] (IntOpExp "*" (IntOpExp "+" (VarExp vv1) (VarExp vv2)) (IntOpExp "+" (VarExp vv3) (VarExp vv4)))
     in eval (AppExp app [IntExp i1, IntExp i2, IntExp i3, IntExp i4]) [] == IntVal ((i1 + i2) * (i3 + i4))
    where types = (v1 :: String, v2 :: String , i1 :: Integer, i2 :: Integer
                 , v3 :: String, v4 :: String , i3 :: Integer, i4 :: Integer)
