module RecursiveDescentSpec where

import Test.Hspec
import Test.QuickCheck
import RecursiveDescent

spec :: Spec
spec = describe "Recursive Descent Testing" $ do
    describe "Additive" $ do
        testAddition
        testSubtract

testAddition :: Spec
testAddition =
    describe "Addition" $ do
        it "simple char addition \"5+6\"" $ do
            evalExpr "5+6" `shouldBe` Right (11, "")
        it "simple char addition \"5+6\"" $ do
            evalExpr "5+6+6+6" `shouldBe` Right (23, "")
        it "simple char addition \"5+6+4+8+6+8\"" $ do
            evalExpr "5+6+4+8+6+8" `shouldBe` Right (37, "")
        it "simple char addition \"5+6+4+8+6+8\"" $ do
            evalExpr "5+5+5+5+5+5+5+5+5+5" `shouldBe` Right (50, "")

testSubtract :: Spec
testSubtract =
    describe "Subtract" $ do
        it "simple char subtract \"-6\"" $ do
            evalExpr "-6" `shouldBe` Right (-6, "")
        it "simple char subtract \"5-6\"" $ do
            evalExpr "5-6" `shouldBe` Right (-1, "")
        it "simple char subtract \"5-6-5\"" $ do
            evalExpr "5-6-5" `shouldBe` Right (-6, "")
        it "simple char subtract \"10-6-5\"" $ do
            evalExpr "10-6-5" `shouldBe` Right (-1, "")
        it "simple char subtract \"5-6-4-8-6-8\"" $ do
            evalExpr "5-6-4-8-6-8" `shouldBe` Right (-27, "")
