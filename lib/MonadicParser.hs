module MonadicParser where

import Control.Applicative
import BootstrapJust

--------

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Number Float
          | Fail String
          deriving (Show, Eq, Ord)

--parseNum :: Parser Float
--parseNum = parseSpacedChar '+' *> parseFloat <|> parseSpaced parseFloat
parseNum :: Parser Expr
--parseNum =  (Number <$> parseFloat)
parseNum = parseChar '+' *> (Number <$> parseFloat) <|> (Number <$> parseFloat)

additive :: Parser (Expr -> Expr -> Expr)
additive = do
    parseSpacedChar '+'
    return Add
    <|> do
    parseSpacedChar '-'
    return Sub

multitive :: Parser (Expr -> Expr -> Expr)
multitive = do
    parseSpacedChar '*'
    return Mul
    <|> do
    parseSpacedChar '/'
    return Div

expr :: Parser Expr
expr  = term `chainLeftAssociative'` additive

term :: Parser Expr
term = factor `chainLeftAssociative'` multitive

factor :: Parser Expr
--factor = parseNum <|>  parens expr
factor = parens expr <|> parseNum

parens :: Parser Expr -> Parser Expr
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

eval :: Expr -> Float
eval e = case e of
    Add a b         -> eval a + eval b
    Sub a b         -> eval a - eval b
    Mul a b         -> eval a * eval b
    Div a (Number 0)-> error "Cannot Divide by zero"
    Div a b         -> eval a / eval b
    Number n        -> n
    Fail s          -> error s


evalExpr :: String -> Maybe Float
evalExpr s = case runParser expr s of
    Just (a, [])    -> Just (eval a)
    _               -> Nothing

