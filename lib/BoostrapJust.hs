module BoostrapJust where

import Control.Applicative


data Parser a = Parser {
    runParser :: String -> Maybe (a , String)
}

parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Just (c, xs)
            | otherwise = Nothing
        fct [] = Nothing


parseAnyChar :: String -> Parser Char
parseAnyChar str@(a:as) = parseOr (parseChar a) (parseAnyChar as)
parseAnyChar [] = Parser (\x -> Nothing)


parseOr :: Parser a -> Parser a -> Parser a
parseOr one two = Parser fct
    where
        fct s = case runParser one s of
            Nothing -> runParser two s
            r -> r


parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd first second = parseAndWith (\x y -> (x,y)) first second


parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith with first second = Parser fct
    where
        fct s = case runParser first s of
            Just (r, left) -> case runParser second left of
                Just (r', left')    -> Just (with r r', left')
                Nothing             -> Nothing
            Nothing -> Nothing


parseMany :: Parser a -> Parser [a]
parseMany parser = Parser fct
    where
        fct s = case runParser (parseAndWith (:) parser (parseMany parser)) s of
            Nothing -> Just ([], s)
            r       -> r


parseSome :: Parser a -> Parser [a]
parseSome parser = Parser fct
    where
        fct s = case runParser (parseAnd parser (parseMany parser)) s of
            Just (a, as)   -> Just (uncurry (:) a, as)
            _               -> Nothing


parseUInt :: Parser Int
parseUInt = Parser fct
    where
        fct s = case runParser (parseSome $ parseAnyChar ['0'..'9']) s of
            Just (x, xs) -> Just (read x :: Int, xs)
            _ -> Nothing

parseInt :: Parser Int
parseInt  = Parser fct
    where
        fct s = case runParser (parseChar '-') s of
            -- TODO: Negate
            Just (_, xs)   -> case runParser parseUInt xs of
                Just (x, xs')   -> Just (negate x, xs')
                r               -> r
            _               -> runParser parseUInt s

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Just (a, s') -> Just (f a, s')
        Nothing      -> Nothing

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser p1 <*> pp2 = Parser $ \s -> case p1 s of
        Just (f, s') -> case runParser pp2 s' of
            Just (a, s'') -> Just (f a, s'')
            Nothing       -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
--    (<|>) = parseOr
    (Parser p1) <|> (Parser p2) = Parser fct
        where
            fct s = case p1 s of
                Nothing -> p2 s
                r -> r

--    (Parser p1) <|> pp2 = Parser $ \s -> p1 s <|> runParser pp2 s

--parseSpaced :: Parser a -> Parser a
--parseSpaced p = many (parseChar ' ') *> p <* many (parseChar ' ')
--
--parseTuple :: Parser a -> Parser (a, a)
--parseTuple p = parseOpenPar *> parseTuple' <* parseClosePar
--    where
--        parseTuple'     = parseElem <|> Parser (\x -> Nothing)
----        parse
--        parseElem       = (\x y -> (x, y)) <$> p <*> (parseChar ',' *> p)
--        parseOpenPar    = parseSpaced (parseChar '(')
--        parseClosePar   = parseSpaced (parseChar ')')
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
