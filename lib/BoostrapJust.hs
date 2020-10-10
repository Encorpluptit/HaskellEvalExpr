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

-- Using Alternative <|> (parseOr) to parse a char or the rest of the string
parseAnyChar :: String -> Parser Char
parseAnyChar (a:as) = parseChar a <|> parseAnyChar as
parseAnyChar [] = Parser (\x -> Nothing)

-- Using ParseAndWith with lambda (prelude possible ?) to apply both parser and get a tuple of parsed elements
parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd = parseAndWith (\x y -> (x,y))

-- Using Applicative Functor to apply Parser a and Parser b with the fct given in parameter
-- See Applicative Functor
parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith fct first second = fct <$> first <*> second

-- Using recursive to parse with Parser a and adding each parsed element to a list of parsed elements
-- (:) -> fct that take 2 args (an elem and a list) and prepend (insert before) that elem to the list.
-- (:) :: a -> [a] -> [a]
parseMany :: Parser a -> Parser [a]
parseMany parser = Parser fct
    where
        fct s = case runParser (parseAndWith (:) parser (parseMany parser)) s of
            Nothing -> Just ([], s)
            r       -> r

-- Using fmap infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by parseAnd
-- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
-- See Functor (fmap)
parseSome :: Parser a -> Parser [a]
parseSome parser = uncurry (:) <$> parseAnd parser (parseMany parser)


-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
        parseNegInt = fct <$> (parseChar '-') <*> parseUInt
        fct c = negate


instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Just (a, s') -> Just (f a, s')
        Nothing      -> Nothing

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
--        Just (f, s') -> p2 <$> Just (f s', s')
        Just (f, s') -> case p2 s' of
            Just (a, s'') -> Just (f a, s'')
            Nothing       -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

--instance Alternative Parser where
--    empty = Parser $ const Nothing
--    (Parser p1) <|> pp2 = Parser $ \s -> p1 s <|> runParser pp2 s
----    (<|>) = parseOr
--    (Parser p1) <|> (Parser p2) = Parser fct
--        where
--            fct s = case p1 s of
--                Nothing -> p2 s
--                r -> r

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
