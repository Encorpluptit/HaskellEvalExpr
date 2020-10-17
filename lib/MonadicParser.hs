module MonadicParser where

import Data.Char(isSpace, isAlpha, isAlphaNum, isDigit, ord)
import Control.Applicative
import Control.Monad

data Parser a = Parser {
    runParser :: String -> [(a, String)]
}

instance Monad Parser where
--    return x = Parser $ \s -> Just (x, s)
--    Parser a >>= f = Parser fct
--        where
--            fct s = case a s of
--                Nothing -> Nothing
--                Just (x, xs) -> runParser (f x) xs
--    fail _ = Parser (\s -> Nothing)
    return a = Parser (\cs -> [(a, cs)])
    p >>= k  = Parser (\cs -> concat [runParser (k a) cs' | (a,cs') <- runParser p cs])

instance Functor Parser where
    fmap f p = do x<-p; return (f x)

instance Applicative Parser where
    pure = return
    p1 <*> p2 = do x<-p1; y<-p2; return (x y)

instance Alternative Parser where
--    empty = Parser $ const Nothing
    empty = Parser (\cs -> [])

    -- Using Alternative to apply Parser p1 OR Parser p2
--    (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s
    p <|> q = Parser (\cs -> take 1 (runParser (p +++ q) cs))

    -- Using Functor infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by fct
    -- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
    -- See Functor (fmap)
--    some parser = uncurry (:) <$> fct
--        where
--            fct = (\x y -> (x,y)) <$> parser <*> parseMany parser
    many p = many' p <|> return []

many' :: Parser a -> Parser [a]
many' p = do { a<-p; as<-many p; return (a:as) }

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> runParser p cs ++ runParser q cs)



--parseChar :: Char -> Parser Char
--parseChar c = Parser fct
--    where
--        fct (x:xs)
--            | x == c    = Just (x, xs)
--            | otherwise = Nothing
--        fct [] = Nothing

parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = [(x, xs)]
            | otherwise = []
        fct [] = []

--parseAnyChar :: String -> Parser Char
--parseAnyChar [] = Parser (\x -> Nothing)
--parseAnyChar (a:as) = parseChar a <|> parseAnyChar as

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser (\x -> [])
parseAnyChar (a:as) = parseChar a <|> parseAnyChar as

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']


-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUInt :: Parser Int
parseUInt = read <$> some parseDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
        parseNegInt = (\x -> negate) <$> (parseChar '-') <*> parseUInt


--------

additive :: Parser (Int -> Int -> Int)
additive = do
    parseChar '+'
    return (+)
    <|> do
    parseChar '-'
    return (-)

multitive :: Parser (Int -> Int -> Int)
multitive = do
    parseChar '*'
    return (*)
    <|> do
    parseChar '/'
    return div

expr :: Parser Int
expr  = term `chainLeftAssociative'` additive

term :: Parser Int
term = factor `chainLeftAssociative'` multitive

factor :: Parser Int
factor = parseInt <|>  parens expr

parens :: Parser a -> Parser a
parens p = do
    parseChar '('
    a <- p
    parseChar ')'
    return a

chainLeftAssociative :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainLeftAssociative p op a = (p `chainLeftAssociative'` op) <|> return a

chainLeftAssociative' :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftAssociative' p op = do x <- p; fct x
    where
        fct x = do f <- op
                   b <- p
                   fct (f x b)
                <|> return x
