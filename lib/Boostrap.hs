module Boostrap where

import Data.Maybe

-- STEP 2

-- EX 1.2.1
---------------------------------------------------------------------------------------------------
type Error = String
type Parser a = String -> (Maybe (a , String), Error)

parseChar :: Char -> Parser Char
parseChar c (x:xs)
    | x == c = (Just (c, xs), "")
    | otherwise = (Nothing, "No match found")
parseChar _ [] = (Nothing, "empty haystack input")

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

parseOr :: Parser a -> Parser a -> Parser a
parseOr one two str
-- TODO: Why "res_a /= Nothing" doesn't work ??
    | Data.Maybe.isJust res_a = a
    | Data.Maybe.isJust res_b = b
    | otherwise = (Nothing, "Parsing \"Or\" failed")
        where
            a@(res_a, _) = one str
            b@(res_b, _) = two str

-- TODO: WTF This is not it ???
parseAnyChar :: String -> Parser Char
parseAnyChar (a:b:as) x
    | Data.Maybe.isJust res = parsed
    | otherwise = parseAnyChar as x
        where
            parsed@(res, _) = parseOr (parseChar a) (parseChar b) x
parseAnyChar (a:as) x
    | Data.Maybe.isJust res = parsed
    | otherwise = parseAnyChar as x
        where
            parsed@(res, _) = parseChar a x
parseAnyChar [] _ = (Nothing, "empty needle input")


---------------------------------------------------------------------------------------------------
-- STEP 3
-- EX 1.3.2

parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd a b str
    | Data.Maybe.isJust res_a = (Nothing, "")
    | otherwise = (Nothing, "Parsing \"And\" failed")
        where
            one@(res_a, _) = a str
            two@(res_b, _) = b str
