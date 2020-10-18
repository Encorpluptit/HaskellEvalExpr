module RecursiveDescent where
--module RecursiveDescent (
--    evalExpr
--) where

import Control.Applicative
import Parsing

--------

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Number Float
          | Fail Error
          deriving (Show)

parseNum :: Parser Expr
parseNum = parseSpacedChar '+' *> (Number <$> parseSpaced parseFloat) <|> (Number <$> parseSpaced parseFloat)


-- | -----------------------------------------------------------------------------
-- PEG:
-- | -----------------------------------------------------------------------------

additive :: Parser (Expr -> Expr -> Expr)
additive = (parseSpacedChar '+' >> return Add)
            <|> (parseSpacedChar '-' >> return Sub)

multitive :: Parser (Expr -> Expr -> Expr)
multitive = (parseSpacedChar '*' >> return Mul)
            <|> (parseSpacedChar '/' >> return Div)

expr :: Parser Expr
expr  = term `chainLeftAssociative'` additive

term :: Parser Expr
term = power `chainLeftAssociative'` multitive

power :: Parser Expr
power = rightPower <|> factor
    where
        rightPower = Pow <$> factor <*> (parseSpacedChar '^' *> power)

factor :: Parser Expr
factor = parens expr <|> parseNum

parens :: Parser Expr -> Parser Expr
parens p = do
    parseSpacedChar '('
    a <- p
    parseSpacedChar ')'
    return a

-- | -----------------------------------------------------------------------------
-- 10-3-2:
-- | -----------------------------------------------------------------------------

chainLeftAssociative :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainLeftAssociative p op a = (p `chainLeftAssociative'` op) <|> return a

chainLeftAssociative' :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftAssociative' p op = do x <- p; fct x
    where
        fct x = do {f <- op; b <- p; fct (f x b)} <|> return x

-- | -----------------------------------------------------------------------------
-- PEG Evaluation:
-- | -----------------------------------------------------------------------------

eval :: Expr -> Either Error Float
eval (Div a (Number 0)) = Left "Divizion by zero"
eval (Add a b) = (+) <$> (eval a) <*> (eval b)
eval (Sub a b) = (-) <$> (eval a) <*> (eval b)
eval (Mul a b) = (*) <$> (eval a) <*> (eval b)
eval (Div a b) = (/) <$> (eval a) <*> (eval b)
eval (Pow a b) = (**) <$> (eval a) <*> (eval b)
eval (Number n) = Right n
eval (Fail s) = Left s

evalAST :: Expr -> Result Float
evalAST a = case eval a of
    Right a -> Right (a, [])
    Left a -> Left a

-- | -----------------------------------------------------------------------------
-- Core:
-- | -----------------------------------------------------------------------------

evalExpr :: String -> Result Float
evalExpr s = case runParser expr s of
    Right (a, [])    -> evalAST a
    Right (a, xs)    -> Left $ "[Parsing Failed in AST] Left: " ++ show xs
    Left msg         -> Left msg
