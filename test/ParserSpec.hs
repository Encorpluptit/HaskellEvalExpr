module ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Boostrap

spec :: Spec
spec = describe "Lib Parser Testing" $ do
    testParseChar
    testParseAnyChar
    describe "Parse Unsigned Int" $ do
        it "parse \"42aaa\" -> Right (42, \"aaa\")" $ do
            runParser parseUInt "42aaa" `shouldBe` Right (42, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUInt "aaa" `shouldSatisfy` isLeft
            -- TODO: More Tests
    describe "Parse Int" $ do
        it "parse \"42aaa\" -> Right (42, \"aaa\")" $ do
            runParser parseInt "42aaa" `shouldBe` Right (42, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUInt "aaa" `shouldSatisfy` isLeft
        it "parse \"-42aaa\" -> Right (-42, \"aaa\")" $ do
            runParser parseInt "-42aaa" `shouldBe` Right (-42, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseUInt "aaa" `shouldSatisfy` isLeft
            -- TODO: More Tests
    describe "Parse Unsigned Float" $ do
        it "parse \"42.0aaa\" -> Right (42.0, \"aaa\")" $ do
            runParser parseFloat "42.0aaa" `shouldBe` Right (42.0, "aaa")
        it "parse \"42.01aaa\" -> Right (42.01, \"aaa\")" $ do
            runParser parseFloat "42.01aaa" `shouldBe` Right (42.01, "aaa")
        it "parse \"a42aaa\" -> Left _" $ do
            runParser parseFloat "a42aaa" `shouldSatisfy` isLeft
            -- TODO: More Tests
    testParseFloat
--            -- TODO: More Tests

testParseChar::Spec
testParseChar =
    describe "Parse Char" $ do
        it "parse 'a' in \"abcd\" -> Right ('a', \"bcd\")" $ do
            runParser (parseChar 'a') "abcd" `shouldBe` Right ('a', "bcd")
        it "parse 'z' in \"abcd\" -> Left _" $ do
            runParser (parseChar 'z') "abcd" `shouldSatisfy` isLeft
        it "parse 'a' in \"aaaa\" -> Right ('a', \"aaa\")" $ do
            runParser (parseChar 'a') "aaaa" `shouldBe` Right ('a', "aaa")
        it "parse 'b' in \"baaa\" -> Right ('b', \"aaa\")" $ do
            runParser (parseChar 'b') "baaa" `shouldBe` Right ('b', "aaa")
        it "parse 'b' in \"bcda\" -> Right ('b', \"cda\")" $ do
            runParser (parseChar 'b') "bcda" `shouldBe` Right ('b', "cda")
        it "parse 'b' in \"abcd\" -> Left _" $ do
            runParser (parseChar 'b') "abcd" `shouldSatisfy` isLeft

testParseAnyChar::Spec
testParseAnyChar =
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

testParseFloat::Spec
testParseFloat =
    describe "Parse Float" $ do
        it "parse \"42.0aaa\" -> Right (42.0, \"aaa\")" $ do
            runParser parseFloat "42.0aaa" `shouldBe` Right (42.0, "aaa")
        it "parse \"-42.0aaa\" -> Right (-42, \"aaa\")" $ do
            runParser parseFloat "-42.0aaa" `shouldBe` Right (-42.0, "aaa")
        it "parse \"42.01aaa\" -> Right (42.01, \"aaa\")" $ do
            runParser parseFloat "42.01aaa" `shouldBe` Right (42.01, "aaa")
        it "parse \"-42.01aaa\" -> Right (-421, \"aaa\")" $ do
            runParser parseFloat "-42.01aaa" `shouldBe` Right (-42.01, "aaa")
        it "parse \"aaa\" -> Left _" $ do
            runParser parseFloat "aaa" `shouldSatisfy` isLeft
        it "parse \"a42aaa\" -> Left _" $ do
            runParser parseFloat "a42aaa" `shouldSatisfy` isLeft
