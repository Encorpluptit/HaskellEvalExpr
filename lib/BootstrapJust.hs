module BootstrapJust where

import Control.Applicative
import Control.Monad

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
--parseAnyChar [] = Parser (\x -> Nothing)
parseAnyChar [] = empty
parseAnyChar (a:as) = parseChar a <|> parseAnyChar as

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
        parseNegFloat = (negate <$ parseChar '-') <*> parseUFloat

parseUDouble :: Parser Double
parseUDouble = (read::String->Double) <$> some parseFloatDigit

parseDouble :: Parser Double
parseDouble = parseNegDouble <|> parseUDouble
    where
        parseNegDouble = (negate <$ parseChar '-') <*> parseUDouble

parseSpaced :: Parser a -> Parser a
parseSpaced p = many (parseAnyChar "\t ") *> p <* many (parseAnyChar "\t ")
--parseSpaced p = some (parseAnyChar "\t ") *> p <* some (parseAnyChar "\t ")

parseSpacedChar :: Char -> Parser Char
parseSpacedChar c = parseSpaced $ parseChar c

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = openPar *> parseTuple' <* closePar
    where
--        parseTuple'     = parseElem <|> Parser (\x -> Nothing)
        parseTuple'     = parseElem <|> empty
        parseElem       = (\x y -> (x, y)) <$> p <*> comma
        openPar         = parseSpacedChar '('
        closePar        = parseSpacedChar ')'
        comma           = parseSpacedChar ',' *> p

--instance Functor Parser where
--    fmap f (Parser p) = Parser $ \s -> case p s of
--        Just (a, s') -> Just (f a, s')
--        Nothing      -> Nothing

--------------------------------------------------------------------------------
--- Applicative Functor: implement pure and
--------------------------------------------------------------------------------

--instance Applicative Parser where
--    pure x = Parser $ \s -> Just (x, s)
--
--    -- Using Applicative to apply Parser p1 AND Parser p2
--    Parser p1 <*> pp2 = Parser $ \s -> case p1 s of
--        Just (f, s') -> case runParser pp2 s' of
--            Just (a, s'') -> Just (f a, s'')
--            Nothing       -> Nothing
--        Nothing -> Nothing


-- | -----------------------------------------------------------------------------
-- Alternative Functor:
-- Implement:
--      - empty
--      - <|>
--      - some
--      - many
-- | -----------------------------------------------------------------------------

instance Alternative Parser where
    empty = Parser $ const Nothing

    -- Using Alternative to apply Parser p1 OR Parser p2
    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

    -- Using Functor infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by fct
    -- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
    -- See Functor (fmap)
--    some parser = uncurry (:) <$> fct
--        where
--            fct = (\x y -> (x,y)) <$> parser <*> parseMany parser
--
--    many  = parseMany

-- Using recursive to parse with Parser a and adding each parsed element to a list of parsed elements
-- (:) -> fct that take 2 args (an elem and a list) and prepend (insert before) that elem to the list.
-- (:) :: a -> [a] -> [a]
parseMany :: Parser a -> Parser [a]
parseMany parser = Parser fct
    where
        fct s = case runParser ((:) <$> parser <*> parseMany parser) s of
            Nothing -> Just ([], s)
            r       -> r

-- | -----------------------------------------------------------------------------
-- Monad Parser
-- Implement:
--      - Bind Operator: to work with contexts (here Maybe)
--      - Return: Same as Pure, return the monadic Value
--      - Fail: Message in case of Fail Message
-- | -----------------------------------------------------------------------------

instance Monad Parser where
    return x = Parser $ \s -> Just (x, s)
    Parser a >>= f = Parser fct
        where
            fct s = case a s of
                Nothing -> Nothing
                Just (x, xs) -> runParser (f x) xs
--    fail _ = Parser (\s -> Nothing)



-- | -----------------------------------------------------------------------------
-- Once Monad is implemented, We can rewrite our Functor like this
-- | -----------------------------------------------------------------------------

instance Functor Parser where
    fmap f p = do x<-p; return (f x)

instance Applicative Parser where
    pure = return
    p1 <*> p2 = do x<-p1; y<-p2; return (x y)


--instance MonadPlus Parser where
--    mzero             = Parser (\s -> Nothing)
