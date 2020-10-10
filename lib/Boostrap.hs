module Boostrap where

import Data.Either
import Control.Applicative

type Error = String
type Result a = (a , String)

data Parser a = Parser {
    runParser :: String -> Either Error (Result a)
}

parseChar :: Char -> Parser Char
parseChar c = Parser fct
    where
        fct (x:xs)
            | x == c    = Right (c, xs)
            | otherwise = Left $ "ParseChar failed {Left:" ++ xs ++ "}"
        fct [] = Left "ParseChar failed: Empty or End of List"

parseDigit :: Parser Char
parseDigit = parseAnyChar ['0'..'9']

-- Using Alternative <|> (parseOr) to parse a char or the rest of the string
parseAnyChar :: String -> Parser Char
parseAnyChar (a:as) = parseChar a <|> parseAnyChar as
parseAnyChar [] = Parser (\x -> Left "ParseAnyChar failed: Empty or End of List")

--parseOr :: Parser a -> Parser a -> Parser a
--parseOr one two = Parser fct
--    where
--        fct s = case runParser one s of
--            Left _ -> runParser two s
--            r -> r

--parseAnd :: Parser a -> Parser b -> Parser (a,b)
--parseAnd = parseAndWith (\x y -> (x,y))
--
--parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
--parseAndWith with first second = Parser fct
--    where
--        fct s = case runParser first s of
--            Right (r, left) -> case runParser second left of
--                Right (r', left') -> Right (with r r', left')
--                Left msg -> Left $ "ParseAndWith failed at second parser {Left:" ++ left ++ "}"
--            Left msg -> Left $ "ParseAndWith failed at first parser {Left:" ++ s ++ "}"

--parseMany :: Parser a -> Parser [a]
--parseMany parser = Parser fct
--    where
--        fct s = case runParser (parseAndWith (:) parser (parseMany parser)) s of
--            Left _  -> Right ([], s)
--            r       -> r

-- Using fmap infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by parseAnd
-- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
-- See Functor (fmap)
--parseSome :: Parser a -> Parser [a]
--parseSome parser = uncurry (:) <$> parseAnd parser (parseMany parser)

--parseSome :: Parser a -> Parser [a]
--parseSome parser = Parser fct
--    where
--        fct s = case runParser (parseAnd parser (parseMany parser)) s of
--            Right (a, as)   -> Right (uncurry (:) a, as)
--            _               -> Left $ "ParseSome failed {Left:" ++ s ++ "}"

-- Using fmap infix notation to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseUInt :: Parser Int
parseUInt = read <$> some parseDigit

-- Using Alternative Functor to read Int from String (ghc understand itself String->Int) on the digits chars parsed by parseSome.
parseInt :: Parser Int
parseInt = parseNegInt <|> parseUInt
    where
        parseNegInt = (\x -> negate) <$> (parseChar '-') <*> parseUInt

parseSpaced :: Parser a -> Parser a
parseSpaced p = many (parseChar ' ') *> p <* many (parseChar ' ')

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = openPar *> parseTuple' <* closePar
    where
        parseTuple'     = parseElem <|> Parser (\x -> Left "Parsing Tuple Failed")
--        parseElem       = (\x y -> (x, y)) <$> p <*> (parseChar ',' *> p)
        parseElem       = (\x y -> (x, y)) <$> p <*> (comma *> p)
        openPar         = parseSpaced $ parseChar '('
        closePar        = parseSpaced $ parseChar ')'
        comma           = parseSpaced $ parseChar ','


instance Functor Parser where
    fmap f (Parser p) = Parser fct
        where
            fct s = case p s of
                Right (x, xs) -> Right (f x, xs)
                Left b -> Left b

instance Applicative Parser where
    pure p = Parser $ \x -> Right (p, x)

    -- Using Applicative to apply Parser p1 AND Parser p2
    Parser p1 <*> Parser p2 = Parser fct
        where
            fct s = case p1 s of
                Right (f, left) -> case p2 left of
                    Right (a, left')  -> Right (f a, left')
                    Left msg        -> Left msg
                Left msg -> Left msg

instance Alternative Parser where
    empty = Parser $ const $ Left "parser Empty"

    -- Using Alternative to apply Parser p1 OR Parser p2
    Parser p1 <|> Parser p2 = Parser fct
        where
            fct s = case p1 s of
                Left _ -> case p2 s of
                    Right a -> Right a
                    r'        -> r'
                r -> r

    -- Using Functor infix notation to parse with the fct contained in Parser a and apply (:) to the tuple returned by fct
    -- uncurry -> fct that take 2 args (a fct with 2 params and a tuple) to and apply this fct with the elems of this tuple as args of the fct.
    -- uncurry :: (a -> b -> c) -> (a, b) -> c
    -- Here unpack the tuple (a,b) and apply the fct a -> b -> c with a first arg and b second arg
    -- See Functor (fmap)
--    some parser = uncurry (:) <$> parseAnd parser (parseMany parser)
--    some parser = uncurry (:) <$> ((\x y -> (x,y)) <$> parser <*> parseMany parser)
    some parser = uncurry (:) <$> fct
        where
            fct = (\x y -> (x,y)) <$> parser <*> many parser

    -- Using recursive to parse with Parser a and adding each parsed element to a list of parsed elements
    -- (:) -> fct that take 2 args (an elem and a list) and prepend (insert before) that elem to the list.
    -- (:) :: a -> [a] -> [a]
    many parser = Parser fct
        where
            fct s = case runParser ((:) <$> parser <*> many parser) s of
                Left _  -> Right ([], s)
                r       -> r

--parseMany :: Parser a -> Parser [a]
--parseMany parser = Parser fct
--    where
--        fct s = case runParser ((:) <$> parser <*> parseMany parser) s of
--            Left _  -> Right ([], s)
--            r       -> r

-- https://stackoverflow.com/questions/44472008/why-is-there-no-alternative-instance-for-either-but-a-semigroup-that-behaves-sim
-- https://gitlab.haskell.org/ghc/ghc/-/issues/9588
-- https://gitlab.haskell.org/ghc/ghc/-/issues/12160
-- https://gitlab.haskell.org/ghc/ghc/-/issues/9588
--instance (Monoid a)=> Alternative (Either a) where
--    empty = Left mempty
--    L
--instance Alternative Either Error where
--    empty = Left "parser Empty"
-- TODO: AST
