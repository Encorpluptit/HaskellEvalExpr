module MonadicParser where

import Control.Applicative
import Bootstrap

--------

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Number Float
          | Fail String
          deriving (Show, Eq, Ord)

instance Num Expr where
    (Number a) + (Number b) = Number (a + b)
    (Number a) - (Number b) = Number (a - b)
    (Number a) * (Number b) = Number (a * b)
    fromInteger a = Number $ fromInteger a
    abs (Number a) = Number $ abs a
    signum (Number a)
        | a < 0 = -1
        | a > 0 = 1
        | a == 0 = 0

instance Fractional Expr where
    fromRational a = Number $ fromRational a
    (Number a) / (Number b) = Number (a / b)

parseNum :: Parser Expr
parseNum = parseSpacedChar '+' *> (Number <$> parseSpaced parseFloat) <|> (Number <$> parseSpaced parseFloat)

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

eval :: Expr -> Expr
eval e = case e of
    Add a b         -> eval a + eval b
    Sub a b         -> eval a - eval b
    Mul a b         -> eval a * eval b
    Div a (Number 0)-> Fail "Cannot Divide by zero"
    Div a b         -> eval a / eval b
    Number n        -> Number n
    Fail s          -> error s

evalAST :: Expr -> Result Float
evalAST a = case eval a of
      Number res  -> Right (res, [])
      Fail s      -> Left s
      _           -> Left "Eval AST Error"

getAST :: String -> Result Expr
getAST s = runParser expr s

evalExpr :: String -> Result Float
evalExpr s = case getAST s of
    Right (a, [])    -> evalAST a
    Right (a, xs)    -> Left $ "[Parsing Failed in AST] Left: " ++ show xs
    Left msg         -> Left msg
