module ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Boostrap

spec :: Spec
spec = describe "Lib Parser Testing" $ do
    testParseChar
    describe "Parse Any Char" $ do
        it "parse \"abcd\" in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseAnyChar "abcd") "abcd" `shouldBe` Right ('a', "bcd")
        it "parse \"bcda\" in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseAnyChar "bcda") "abcd" `shouldBe` Right ('a', "bcd")
        it "parse \"wxyz\" in \"abcd\" -> Left _" $ do
            runParser (parseAnyChar "wxyz") "abcd" `shouldSatisfy` isLeft
        it "parse 'z' in \"abcd\"" $ do
            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
        it "parse 'a' in \"aaaa\"" $ do
            runParser (parseChar 'a') "aaaa" `shouldBe` Right ('a', "aaa")
        it "parse 'b' in \"baaa\"" $ do
            runParser (parseChar 'b') "baaa" `shouldBe` Right ('b', "aaa")
        it "parse 'b' in \"bcda\"" $ do
            runParser (parseChar 'b') "bcda" `shouldBe` Right ('b', "cda")
        it "parse 'b' in \"abcd\"" $ do
            runParser (parseChar 'b') "abcd" `shouldSatisfy` isLeft
