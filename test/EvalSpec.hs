module EvalSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import RecursiveDescent

spec :: Spec
spec = describe "Eval Testing" $ do
    testAddition
    testSubtract
    testAddSub
    testMultiply
    testDivision
    testMulDiv

testAddition :: Spec
testAddition =
    describe "Addition" $ do
        it "simple addition \"5\" -> Right (Number 5.0, \"\")" $ do
            (run expr "5") `shouldBe` (Right (Number 5.0, ""))
        it "medium addition \"5+6\" -> Right (Add (Number 5.0) (Number 6.0), \"\")" $ do
            (run expr "5+6") `shouldBe` Right (Add (Number 5.0) (Number 6.0), "")
        it "long addition \"5+6+3\" -> Right (Add (Add (Number 5.0) (Number 6.0)) (Number 3.0),\"\")" $ do
            (run expr "5+6+3") `shouldBe` Right (Add (Add (Number 5.0) (Number 6.0)) (Number 3.0),"")
        it "parenthensis addition \"(5+6)+3\" -> Right (Add (Add (Number 5.0) (Number 6.0)) (Number 3.0),\"\")" $ do
            (run expr "(5+6)+3") `shouldBe` Right (Add (Add (Number 5.0) (Number 6.0)) (Number 3.0),"")

testSubtract :: Spec
testSubtract =
    describe "Subtract" $ do
        it "simple subtract \"5\" -> Right (Number 5.0, \"\")" $ do
            (run expr "5") `shouldBe` (Right (Number 5.0, ""))
        it "medium subtract \"5-6\" -> Right (Sub (Number 5.0) (Number 6.0),\"\")" $ do
            (run expr "5-6") `shouldBe` Right (Sub (Number 5.0) (Number 6.0),"")
        it "long subtract \"5-6-3\" -> Right (Sub (Sub (Number 5.0) (Number 6.0)) (Number 3.0),\"\")" $ do
            (run expr "5-6-3") `shouldBe` Right (Sub (Sub (Number 5.0) (Number 6.0)) (Number 3.0),"")
        it "parenthensis subtract \"(5-6-3\" -> Right (Sub (Sub (Number 5.0) (Number 6.0)) (Number 3.0),\"\")" $ do
            (run expr "(5-6)-3") `shouldBe` Right (Sub (Sub (Number 5.0) (Number 6.0)) (Number 3.0),"")

testAddSub :: Spec
testAddSub =
    describe "Mix Addition and Subtract" $ do
        it "Mix \"(5+6)-3\" -> Right (Sub (Add (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5+6)-3") `shouldBe` Right (Sub (Add (Number 5.0) (Number 6.0)) (Number 3.0),"")
        it "Mix \"(5-6)+3\" -> Right (Add (Add (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5-6)+3") `shouldBe` Right (Add (Sub (Number 5.0) (Number 6.0)) (Number 3.0),"")
        it "Mix \"(5+6)-3-2\" -> Right (Sub (Sub (Add (Number 5.0) (Number 6.0)) (Number 3.0)) (Number 2.0), \"\")" $ do
            (run expr "(5+6)-3-2") `shouldBe` Right (Sub (Sub (Add (Number 5.0) (Number 6.0)) (Number 3.0)) (Number 2.0),"")
        it "Mix \"(5-6)+3-2\" -> Right (Sub (Add (Sub (Number 5.0) (Number 6.0)) (Number 3.0)) (Number 2.0),\"\")" $ do
            (run expr "(5-6)+3-2") `shouldBe` Right (Sub (Add (Sub (Number 5.0) (Number 6.0)) (Number 3.0)) (Number 2.0),"")
        it "Mix \"(5-6)-(3-2)\" -> Right (Sub (Sub (Number 5.0) (Number 6.0)) (Sub (Number 3.0) (Number 2.0)),\"\")" $ do
            (run expr "(5-6)-(3-2)") `shouldBe` Right (Sub (Sub (Number 5.0) (Number 6.0)) (Sub (Number 3.0) (Number 2.0)),"")
        it "Mix \"(5-6)+(3-2)\" -> Right (Add (Sub (Number 5.0) (Number 6.0)) (Sub (Number 3.0) (Number 2.0)),\"\")" $ do
            (run expr "(5-6)+(3-2)") `shouldBe` Right (Add (Sub (Number 5.0) (Number 6.0)) (Sub (Number 3.0) (Number 2.0)),"")

testMultiply :: Spec
testMultiply =
    describe "Multiply" $ do
        it "simple Multiply \"5\" -> Right (Number 5.0, \"\")" $ do
            (run expr "5") `shouldBe` (Right (Number 5.0, ""))
        it "medium Multiply \"5*6\" -> Right (Mul (Number 5.0) (Number 6.0), \"\")" $ do
            (run expr "5*6") `shouldBe` Right (Mul (Number 5.0) (Number 6.0),"")
        it "long Multiply \"5*6*3\" -> Right (Mul (Mul (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "5*6*3") `shouldBe` Right (Mul (Mul (Number 5.0) (Number 6.0)) (Number 3.0), "")
        it "parenthesis Multiply \"(5*6)*3\" -> Right (Mul (Mul (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5*6)*3") `shouldBe` Right (Mul (Mul (Number 5.0) (Number 6.0)) (Number 3.0), "")

testDivision :: Spec
testDivision =
    describe "Division" $ do
        it "simple Division \"5\" -> Right (Number 5.0, \"\")" $ do
            (run expr "5") `shouldBe` (Right (Number 5.0, ""))
        it "medium Division \"5/6\" -> Right (Div (Number 5.0) (Number 6.0), \"\")" $ do
            (run expr "5/6") `shouldBe` Right (Div (Number 5.0) (Number 6.0), "")
        it "long Division \"5/6/3\" -> Right (Div (Div (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "5/6/3") `shouldBe` Right (Div (Div (Number 5.0) (Number 6.0)) (Number 3.0), "")
        it "parenthensis Division \"(5/6)/3)\" -> Right (Div (Div (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5/6)/3") `shouldBe` Right (Div (Div (Number 5.0) (Number 6.0)) (Number 3.0), "")
        it "parenthensis Division \"5/(6/3))\" -> Right (Div (Number 5.0) (Div (Number 6.0) (Number 3.0)), \"\")" $ do
            (run expr "5/(6/3)") `shouldBe` Right (Div (Number 5.0) (Div (Number 6.0) (Number 3.0)), "")

testMulDiv :: Spec
testMulDiv =
    describe "Mix Multiply and Division" $ do
        it "Mix \"(5/6)*3\" -> Right (Mul (Div (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5/6)*3") `shouldBe` Right (Mul (Div (Number 5.0) (Number 6.0)) (Number 3.0), "")
        it "Mix \"(5*6)/3\" -> Right (Div (Mul (Number 5.0) (Number 6.0)) (Number 3.0), \"\")" $ do
            (run expr "(5*6)/3") `shouldBe` Right (Div (Mul (Number 5.0) (Number 6.0)) (Number 3.0), "")
        it "Mix \"(5/6)*(3*2)\" -> Right (Mul (Div (Number 5.0) (Number 6.0)) (Mul (Number 3.0) (Number 2.0)), \"\")" $ do
            (run expr "(5/6)*(3*2)") `shouldBe` Right (Mul (Div (Number 5.0) (Number 6.0)) (Mul (Number 3.0) (Number 2.0)), "")
        it "Mix \"(5/6)*(3/2)\" -> Right (Mul (Div (Number 5.0) (Number 6.0)) (Div (Number 3.0) (Number 2.0)), \"\")" $ do
            (run expr "(5/6)*(3/2)") `shouldBe` Right (Mul (Div (Number 5.0) (Number 6.0)) (Div (Number 3.0) (Number 2.0)), "")
        it "Mix \"(5/6)/(3*2)\" -> Right (Div (Div (Number 5.0) (Number 6.0)) (Mul (Number 3.0) (Number 2.0)), \"\")" $ do
            (run expr "(5/6)/(3*2)") `shouldBe` Right (Div (Div (Number 5.0) (Number 6.0)) (Mul (Number 3.0) (Number 2.0)), "")
        it "Mix \"(5/6)/(3/2)\" -> Right (Div (Div (Number 5.0) (Number 6.0)) (Div (Number 3.0) (Number 2.0)), \"\")" $ do
            (run expr "(5/6)/(3/2)") `shouldBe` Right (Div (Div (Number 5.0) (Number 6.0)) (Div (Number 3.0) (Number 2.0)), "")
