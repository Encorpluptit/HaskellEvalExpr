module EvalSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Boostrap

spec :: Spec
spec = describe "Eval Testing" $ do
    describe "tesing" $ do
        it "parse 'a' in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseChar 'a') "abcd" `shouldBe` Right ('a', "bcd")
        it "parse 'z' in \"abcd\" -> Left _" $ do
            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
