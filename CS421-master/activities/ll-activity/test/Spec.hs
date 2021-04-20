import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Lib
import Test.QuickCheck
import Control.Monad

main :: IO ()
main = defaultMain tests

instance Arbitrary Exp where
  arbitrary = sized arbitraryExp

arbitraryExp :: Int -> Gen Exp
arbitraryExp n | n < 2 = oneof [ randIntExp
                               , randVarExp
                               ]
arbitraryExp n = oneof [randPlusExp n
                       ,randLetExp n]


randPlusExp :: Int -> Gen Exp
randPlusExp n = do
  n1 <- choose (1,n-1)
  let n2 = n - n1
  e1 <- arbitraryExp n1
  e2 <- arbitraryExp n2
  return (PlusExp e1 e2)

randLetExp :: Int -> Gen Exp
randLetExp n = do
  n1 <- choose (1,n-1)
  let n2 = n - n1
  v <- choose ('a','z')
  e1 <- arbitraryExp n1
  e2 <- arbitraryExp n2
  return (LetExp [v] e1 e2)

randIntExp :: Gen Exp
randIntExp = do Positive x <- arbitrary
                return $ IntExp x

randVarExp :: Gen Exp
randVarExp = do x <- choose ('a','z')
                return $ VarExp [x]

expToString (IntExp i) = show i
expToString (VarExp v) = v
expToString (PlusExp e1 e2) = "+ " ++ expToString e1 ++ " " ++ expToString e2
expToString (LetExp v e1 e2) = "let " ++ v ++ " = " ++ expToString e1 ++ " in " ++ expToString e2 ++ " end"

expToParenString (IntExp i) = show i
expToParenString (VarExp v) = v
expToParenString (PlusExp e1 e2) = "+ ( " ++ expToString e1 ++ " ) ( " ++ expToString e2 ++ " )"
expToParenString (LetExp v e1 e2) = "let " ++ v ++ " = ( " ++ expToString e1 ++ " ) in ( " ++ expToString e2 ++ " ) end"
tests = [
        testGroup "Small Expressions Function" [
                testProperty "Can parse integers" (forAll randIntExp parserWorks)
              , testProperty "Can parse variables" (forAll randVarExp parserWorks)
           ],

        testGroup "Large Expressions Function" [
                testProperty "Can parse arbitrary expressions without parens"
                   (forAll arbitrary parserWorks)
              , testProperty "Can parse arbitrary expressions with some parens"
                   (forAll arbitrary parenParserWorks)
           ]
      ]


parserWorks :: Exp -> Bool
parserWorks e = result == e
  where stringRep = expToString e
        (result,_) = parse stringRep

parenParserWorks :: Exp -> Bool
parenParserWorks e = result == e
  where stringRep = expToString e
        pstringRep = expToParenString e
        (result,_) = parse pstringRep




-- data Exp = PlusExp Exp Exp
--          | IntExp Integer
--          | VarExp String
--          | LetExp String Exp Exp
--     deriving Show

