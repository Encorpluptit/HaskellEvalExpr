module ParserSpec where

import  Test.Hspec
import  Test.QuickCheck

spec :: Spec
spec = describe "Parser Testing" $ do

  describe "tesing" $ do
    it "test 01" $ do
      "lol" `shouldBe` "lol"
