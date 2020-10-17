module MonadicParser where

import Control.Applicative
import BootstrapJust

--------

parseNum :: Parser Float
parseNum = parseSpacedChar '+' *> parseFloat <|> parseSpaced parseFloat

additive :: Parser (Float -> Float -> Float)
additive = do
    parseSpacedChar '+'
    return (+)
    <|> do
    parseSpacedChar '-'
    return (-)

multitive :: Parser (Float -> Float -> Float)
multitive = do
    parseSpacedChar '*'
    return (*)
    <|> do
    parseSpacedChar '/'
    return (/)

expr :: Parser Float
expr  = term `chainLeftAssociative'` additive

term :: Parser Float
term = factor `chainLeftAssociative'` multitive

factor :: Parser Float
factor = parseNum <|>  parens expr

parens :: Parser a -> Parser a
parens p = do
    parseSpacedChar '('
    a <- p
    parseSpacedChar ')'
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

evalExpr :: String -> Maybe Float
evalExpr s = case runParser expr s of
    Just (a, [])    -> Just a
    _               -> Nothing

