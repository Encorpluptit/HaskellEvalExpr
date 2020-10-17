module EvalSpec where

import Test.Hspec
import Test.QuickCheck
--import Data.Either
--import RecursiveDescent

spec :: Spec
spec = describe "Eval Testing" $ do
    describe "Addition" $ do
        it "simple char addition \"5+6\"" $ do
            5+6 `shouldBe` 11
--spec = describe "Eval Testing" $ do
--    describe "Additive" $ do
--        testAdditiveAddition
--        testAdditiveSubtract
--
--testAdditiveAddition::Spec
--testAdditiveAddition =
--    describe "Addition" $ do
--        it "simple char addition \"5+6\"" $ do
--            additive "5+6" `shouldBe` Num 11 []
--        it "simple char addition \"5+6+4+8+6+8\"" $ do
--            additive "5+6+4+8+6+8" `shouldBe` Num 37 []
--
--testAdditiveSubtract::Spec
--testAdditiveSubtract =
--    describe "Subtract" $ do
--        it "simple char subtract \"-6\"" $ do
--            additive "-6" `shouldBe` Num (-6) []
--        it "simple char subtract \"5-6\"" $ do
--            additive "5-6" `shouldBe` Num (-1) []
--        it "simple char subtract \"5-6-5\"" $ do
--            additive "5-6-5" `shouldBe` Num (-6) []
--        it "simple char subtract \"10-6-5\"" $ do
--            additive "10-6-5" `shouldBe` Num (-6) []
--        it "simple char subtract \"5-6-4-8-6-8\"" $ do
--            additive "5-6-4-8-6-8" `shouldBe` Num (-27) []
