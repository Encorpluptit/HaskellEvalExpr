module MonadicSpec where

import Test.Hspec
import Test.QuickCheck
import MonadicParser

spec :: Spec
spec = describe "Monadic Testing" $ do
    describe "Additive" $ do
        testAddition
        testSubtract

testAddition::Spec
testAddition =
    describe "Addition" $ do
        it "simple char addition \"5+6\"" $ do
            evalExpr "5+6" `shouldBe` Just 11
        it "simple char addition \"5+6+4+8+6+8\"" $ do
            evalExpr "5+6+4+8+6+8" `shouldBe` Just 37

testSubtract::Spec
testSubtract =
    describe "Subtract" $ do
        it "simple char subtract \"-6\"" $ do
            evalExpr "-6" `shouldBe` Just (-6)
        it "simple char subtract \"5-6\"" $ do
            evalExpr "5-6" `shouldBe` Just (-1)
        it "simple char subtract \"5-6-5\"" $ do
            evalExpr "5-6-5" `shouldBe` Just (-6)
        it "simple char subtract \"10-6-5\"" $ do
            evalExpr "10-6-5" `shouldBe` Just (-1)
        it "simple char subtract \"5-6-4-8-6-8\"" $ do
            evalExpr "5-6-4-8-6-8" `shouldBe` Just (-27)
