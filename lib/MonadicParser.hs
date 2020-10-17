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
    p <*> q = do f<-p; x<-q; return (f x)

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
