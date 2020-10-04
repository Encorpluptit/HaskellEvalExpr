module Boostrap where

import Data.Maybe
import Data.Either

-- STEP 2

-- EX 1.2.1
---------------------------------------------------------------------------------------------------
--type Error = String
--type Parser a = String -> (Maybe (a , String), Error)
--
--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--    | x == c = (Just (c, xs), "")
--    | otherwise = (Nothing, "No match found")
--parseChar _ [] = (Nothing, "empty haystack input")

-- EX 1.2.2

--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:as) x@(b:bs)
--    | a == b = Just (b, bs, "")
--    | otherwise = parseAnyChar as x
--parseAnyChar [] _ = Nothing
--parseAnyChar _ [] = Nothing

-- Just remark why we have done parseChar

--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:as) x
--    | res /= Nothing = parsed
--    | otherwise = parseAnyChar as x
--        where
--            parsed@(res, _) = parseChar a x
--parseAnyChar [] _ = (Nothing, "empty needle input")


---------------------------------------------------------------------------------------------------
-- STEP 3
-- EX 1.3.1


--type Error = String
--data Result a = Result {
--    res::Maybe(a , String),
--    err::Error
--}
--type Parser a = String -> Result a
--
---- TODO: Adding function to pretty print (J'arrive pas à faire une instance deShow pour un type, un newtype, ou data avec un type générique)
--instance Show Result a where
--    show (Result a (str, parser)) = show "coucou"

--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--    | x == c = Result(Just (c, xs), "")
--    | otherwise = Result(Nothing, "No match found")
--parseChar _ [] = Result(Nothing, "empty haystack input")
--
--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:as) x
--    | res /= Nothing = Result(parsed)
--    | otherwise = parseAnyChar as x
--        where
--            Result(parsed@(res, _)) = parseChar a x
--parseAnyChar [] _ = Result(Nothing, "empty needle input")

--printParser :: MP -> String
--printParser mp = "modulus " ++ show (modulus mp) ++ ", coeffs: " ++ printCoeffs (modulus mp) (coeffs mp)

-------------------

-- TODO: Adding utility function to send Error
--failedParsing :: String -> Parser a
--failedParsing err = (Nothing, err)

--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two str
---- TODO: Why "res_a /= Nothing" doesn't work ??
--    | Data.Maybe.isJust res_a = a
--    | Data.Maybe.isJust res_b = b
--    | otherwise = (Nothing, "Parsing \"Or\" failed")
--        where
--            a@(res_a, _) = one str
--            b@(res_b, _) = two str
--
---- TODO: WTF This is not it ???
--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:b:as) x
--    | Data.Maybe.isJust res = parsed
--    | otherwise = parseAnyChar as x
--        where
--            parsed@(res, _) = parseOr (parseChar a) (parseChar b) x
--parseAnyChar (a:as) x
--    | Data.Maybe.isJust res = parsed
--    | otherwise = parseAnyChar as x
--        where
--            parsed@(res, _) = parseChar a x
--parseAnyChar [] _ = (Nothing, "empty needle input")


---------------------------------------------------------------------------------------------------
-- STEP 3
-- EX 1.3.2

type Error = String
type Result a = Maybe (a , String)
type Parser a = String -> Either String (Result a)

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | x == c = Right $ Just (c, xs)
    | otherwise = Left "No match found"
parseChar _ [] = Left "No match found"

parseAnyChar :: String -> Parser Char
parseAnyChar needle@(a:b:as) x
    | Data.Either.isRight parsed = parsed
    | otherwise = parseAnyChar as x
        where
            parsed = parseOr (parseChar a) (parseChar b) x
parseAnyChar (a:[]) x = parseChar a x
parseAnyChar [] _ = Left "No match found"

parseOr :: Parser a -> Parser a -> Parser a
parseOr one two str
    | Data.Either.isRight a = a
    | Data.Either.isRight b = b
    | otherwise = Left "Parsing 'Or' failed"
        where
            a = one str
            b = two str

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd first second str = a
    where
        a = case first str of
            Right _ -> b
            Left msg -> Left msg
        b = case second $ tail str of
            Right _ -> Left "ParseAnd Failed 2l"
--            Right _ -> Right ((fst $ Data.Either.fromRight a, fst $ Data.Either.fromRight b), snd $ Data.Either.fromRight b)
            Left msg -> Left "ParseAnd Failed 2l"

--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd first second str
--    | Data.Either.isRight a = c
--    | otherwise = Left "ParseAnd Failed at a"
--        where
--            a = first str
--            b = second $ tail str
--            c = case Data.Either.isRight b of
--                True -> Right ((fst $ Data.Either.fromRight a, fst $ Data.Either.fromRight b), snd $ Data.Either.fromRight b)
--                False -> Left "ParseAnd Failed at b"

----------------

--type Parser a = String -> Maybe (a , String)
--
--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--    | x == c = Just (c, xs)
--    | otherwise = Nothing
--parseChar _ [] = Nothing
--
--parseAnyChar :: String -> Parser Char
--parseAnyChar needle@(a:b:as) x
--    | Data.Maybe.isJust parsed = parsed
--    | otherwise = parseAnyChar as x
--        where
--            parsed = parseOr (parseChar a) (parseChar b) x
--parseAnyChar (a:[]) x = parseChar a x
--parseAnyChar [] _ = Nothing
--
--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two str
--    | Data.Maybe.isJust a = a
--    | Data.Maybe.isJust b = b
--    | otherwise = Nothing
--        where
--            a = one str
--            b = two str
--
--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd first second str
--    | Data.Maybe.isJust a = c
--    | otherwise = Nothing
--        where
--            a = first str
--            b = second $ tail str
--            c = case Data.Maybe.isJust b of
--                True -> Just ((fst $ fromJust a, fst $ fromJust b), snd $ fromJust b)
--                False -> Nothing

--parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
--parseAndWith

