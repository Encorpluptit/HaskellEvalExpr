module Boostrap where

import Data.Maybe
import Data.Either


-----------------------------------------------------------------------------------------------------
---- STEP 1
---- EX 1.1.1
--type Parser a = String -> Maybe (a , String)
--
-----------------------------------------------------------------------------------------------------
---- STEP 2
---- EX 1.2.1
--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--    | x == c = Just (c, xs)
--    | otherwise = Nothing
--parseChar _ [] = Nothing
--
-----------------------------------------------------------------------------------------------------
---- STEP 2
---- EX 1.2.2
--parseAnyChar :: String -> Parser Char
--parseAnyChar needle@(a:b:as) x
--    | Data.Maybe.isJust parsed = parsed
--    | otherwise = parseAnyChar as x
--        where
--            parsed = parseOr (parseChar a) (parseChar b) x
--parseAnyChar (a:[]) x = parseChar a x
--parseAnyChar [] _ = Nothing
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.1
--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two str
--    | Data.Maybe.isJust a = a
--    | Data.Maybe.isJust b = b
--    | otherwise = Nothing
--        where
--            a = one str
--            b = two str
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.2
--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd first second str@(_:xs) =
--    case first str of
--         Just r -> case second xs of
--            Just rs -> Just ((fst r, fst rs), snd rs)
--            _ -> Nothing
--         _ -> Nothing
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.3
--parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
--parseAndWith fct first second s =
--    case first s of
--        Just (r, rs) -> case second rs of
--            Just (r', rs') -> Just (fct r r', rs')
--            _  -> Nothing
--        _ -> Nothing
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.4
--parseMany :: Parser a -> Parser [a]
--parseMany parser str
--    | isJust res = res
--    | otherwise = Just ([], str)
--        where
--            res = parseAndWith (\x y -> (x:y)) parser (parseMany parser) str
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.5
--parseSome :: Parser a -> Parser [a]
--parseSome parser str
--    | isJust res = Just ((fst a:snd a), as)
--    | otherwise = Nothing
--        where
--            res = parseAnd parser (parseMany parser) str
--            Just (a, as) = res
--
-----------------------------------------------------------------------------------------------------
---- STEP 4
---- EX 1.4.1
--parseUInt :: Parser Int
--parseUInt str
--    | isJust res = Just (read x :: Int, xs)
--    | otherwise = Nothing
--        where
--            res = parseSome (parseAnyChar ['0'..'9']) str
--            Just (x, xs) = res
--
--parseInt :: Parser Int
--parseInt str@(a:as)
--    | isJust neg = Just (negate x, xs)
--    | otherwise = parseUInt str
--        where
--            neg = parseChar '-' str
--            Just (x, xs) = parseUInt as
--


-----------------------------------------------------------------------------------------------------
---- STEP 1
---- EX 1.1.1
--type Error = String
--type Result a = (a , String)
--type Parser a = String -> Either Error (Result a)
--
-----------------------------------------------------------------------------------------------------
---- STEP 2
---- EX 1.2.1
--parseChar :: Char -> Parser Char
--parseChar c (x:xs)
--    | x == c    = Right (c, xs)
--    | otherwise = Left "No match found"
--parseChar _ [] = Left "No match found"
--
-----------------------------------------------------------------------------------------------------
---- STEP 2
---- EX 1.2.2
--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:b:as) x =
--    case parseOr (parseChar a) (parseChar b) x of
--        Left _  -> parseAnyChar as x
--        r       -> r
--parseAnyChar [a] x  = parseChar a x
--parseAnyChar [] _   = Left "No match found"
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.1
--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two str =
--    case one str of
--        Left _  -> case two str of
--            Left _  -> Left "Parsing 'Or' failed"
--            r'      -> r'
--        r -> r
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.2
--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd first second str@(_:xs) =
--    case first str of
--         Right r -> case second xs of
--            Right rs -> Right ((fst r, fst rs), snd rs)
--            Left msg -> Left msg
--         Left msg -> Left msg
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.3
--parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
--parseAndWith fct first second s =
--    case first s of
--        Right (r, rs) -> case second rs of
--            Right (r', rs') -> Right (fct r r', rs')
--            Left msg -> Left msg
--        Left msg -> Left msg
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.4
---- Pattern Guard instead of fct in guard
--parseMany :: Parser a -> Parser [a]
--parseMany parser str =
--    case parseAndWith (:) parser (parseMany parser) str of
--        Left _  -> Right ([], str)
--        r       -> r
--
-----------------------------------------------------------------------------------------------------
---- STEP 3
---- EX 1.3.5
--parseSome :: Parser a -> Parser [a]
--parseSome parser str =
--    case parseAnd parser (parseMany parser) str of
--        Right (a, as)   -> Right (uncurry (:) a, as)
--        _               -> Left $ "ParseSome failed" ++ str
--
-----------------------------------------------------------------------------------------------------
---- STEP 4
---- EX 1.4.1
--parseUInt :: Parser Int
--parseUInt str =
--    case parseSome (parseAnyChar ['0'..'9']) str of
--        Right (x, xs) -> Right (read x :: Int, xs)
--        _ -> Left "Target is not a UInt"
--
--parseInt :: Parser Int
--parseInt str@(a:as) =
--    case parseChar '-' str of
--        Right (x, xs)   -> parseUInt as
--        _               -> parseUInt str
--
--parseTuple :: Parser a -> Parser (a, a)
--parseTuple p s =
--    case begin p s of
--        Right (a, s') -> case middle p s' of
--            Right (b, s'') -> Right ((a, b), s'')
--            Left _ -> Left $ "Failed at second number: " ++ s'
--        Left _ -> Left $ "Failed at first number: " ++ s
--    where
--        begin       = ignore (parseChar '(')
--        middle p    = ignore (parseChar ',') (end p)
--        end p       = parseAndWith const p (parseChar ')')
--        ignore p1 p2 = parseAndWith seq p1 p2




---------------------------------------------------------------------------------------------------
------ PART 2 -------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---- STEP 1
type Error = String
type Result a = (a , String)

data Parser a = Parser {
    runParser :: String -> Either Error (Result a)
}

-----------------------------------------------------------------------------------------------------
---- STEP 1
---- EX 2.2.1
parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Right (c, xs)
            | otherwise = Left "No match found"
        fct [] = Left "No match found"

--parseAnyChar :: String -> Parser Char
--parseAnyChar (a:b:as) x =
--    case parseOr (parseChar a) (parseChar b) x of
--        Left _  -> parseAnyChar as x
--        r       -> r
--parseAnyChar [a] x  = parseChar a x
--parseAnyChar [] _   = Left "No match found"

--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two =
--    case one of
--        Parser {runParser= s -> Left _} -> two
----        Parser {} -> case two of
----            Left _  -> Parser {runParser=Left "Parsing 'Or' failed" }
----            r'      -> Parser r'
--        r -> r
----    where
----        f = Parser
----parseOr :: Parser a -> Parser a -> Parser a
----parseOr one two = one two
