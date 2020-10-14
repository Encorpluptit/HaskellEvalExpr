module BoostrapJust where

import Control.Applicative

type Result a = Maybe (a , String)
data Parser a = Parser {
    runParser :: String -> Result a
}

parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Just (x, xs)
            | otherwise = Nothing
        fct [] = Nothing

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

parseFloatDigit :: Parser Char
parseFloatDigit = parseAnyChar ('.':['0'..'9'])

-- Using Alternative <|> (parseOr) to parse a char or the rest of the string
parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (\x -> Nothing)
parseAnyChar (a:as) = parseChar a <|> parseAnyChar as

-- Using ParseAndWith with lambda (prelude possible ?) to apply both parser and get a tuple of parsed elements
--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd = parseAndWith (\x y -> (x,y))
--parseAnd p1 p2 = (\x y -> (x,y)) <$> p1 <*> p2


-- Using fmap infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by parseAnd
-- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
-- See Functor (fmap)
--parseSome :: Parser a -> Parser [a]
--parseSome parser = uncurry (:) <$> parseAnd parser (parseMany parser)


-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUInt :: Parser Int
parseUInt = read <$> some parseDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
        parseNegInt = (\x -> negate) <$> (parseChar '-') <*> parseUInt
--        parseNegInt = fct <$> (parseChar '-') <*> parseUInt
--        fct c = negate

-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUFloat :: Parser Float
parseUFloat = (read::String->Float) <$> some parseFloatDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseFloat :: Parser Float
parseFloat = parseNegFloat <|> parseUFloat
    where
--        parseNegFloat = const negate <$> parseChar '-' <*> parseUFloat
        parseNegFloat = (negate <$ parseChar '-') <*> parseUFloat

parseSpaced :: Parser a -> Parser a
parseSpaced p = many (parseChar ' ') *> p <* many (parseChar ' ')

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = openPar *> parseTuple' <* closePar
    where
        parseTuple'     = parseElem <|> Parser (\x -> Nothing)
--        parseElem       = (\x y -> (x, y)) <$> p <*> (parseChar ',' *> p)
        parseElem       = (\x y -> (x, y)) <$> p <*> (comma *> p)
        openPar         = parseSpaced $ parseChar '('
        closePar        = parseSpaced $ parseChar ')'
        comma           = parseSpaced $ parseChar ','

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


instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Just (a, s') -> Just (f a, s')
        Nothing      -> Nothing

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)

    -- Using Applicative to apply Parser p1 AND Parser p2
    Parser p1 <*> pp2 = Parser $ \s -> case p1 s of
        Just (f, s') -> case runParser pp2 s' of
            Just (a, s'') -> Just (f a, s'')
            Nothing       -> Nothing
        Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing

    -- Using Alternative to apply Parser p1 OR Parser p2
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

    -- Using Functor infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by fct
    -- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
    -- See Functor (fmap)
--    some parser = uncurry (:) <$> parseAnd parser (parseMany parser)
--    some parser = uncurry (:) <$> ((\x y -> (x,y)) <$> parser <*> parseMany parser)
    some parser = uncurry (:) <$> fct
        where
            fct = (\x y -> (x,y)) <$> parser <*> parseMany parser

    many  = parseMany

-- Using recursive to parse with Parser a and adding each parsed element to a list of parsed elements
-- (:) -> fct that take 2 args (an elem and a list) and prepend (insert before) that elem to the list.
-- (:) :: a -> [a] -> [a]
parseMany :: Parser a -> Parser [a]
parseMany parser = Parser fct
    where
        fct s = case runParser ((:) <$> parser <*> parseMany parser) s of
            Nothing -> Just ([], s)
            r       -> r

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    Parser a >>= f = Parser fct
        where
            fct s = case a s of
                Nothing -> Nothing
                Just (x, xs) -> runParser (f x) xs
    fail _ = Parser (\s -> Nothing)
--                    Nothing -> Nothing
--                    r -> r
--                Just (x, xs) -> case runParser (f x) xs of
--                    Nothing -> Nothing
--                    r -> r

--    Nothing >>= f = Nothing
--    Just x >>= f  = f x
--    fail _ = Nothing
