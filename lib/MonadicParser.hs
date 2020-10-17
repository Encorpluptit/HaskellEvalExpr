module MonadicParser where

import Control.Applicative
import BootstrapJust

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
