module RecursiveDescent where

import Control.Applicative
import Debug.Trace
import Boostrap (
    Parser(..),
    Result,
    Error,
    parseSpaced,
    parseChar,
    parseFloat,
    parseUFloat,
    runParser,
    parseSpaced,
    parseInt)

-- Recursive Descent
--
-- https://www.booleanworld.com/building-recursive-descent-parsers-definitive-guide/


-- Additive and Multiplicative Principles:
--
-- http://www.math.wichita.edu/~hammond/class-notes/section-counting-basics.html
-- https://courses.lumenlearning.com/ivytech-collegealgebra/chapter/using-the-addition-and-multiplication-principles/

data Expr = Num Float
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Power Expr Expr
            | Fail
            deriving (Show, Eq)

parseNum :: Parser Expr
parseNum = parseChar '+' *> (Num <$> parseFloat) <|> (Num <$> parseFloat)

parseSpacedChar :: Char -> Parser Char
parseSpacedChar c = parseSpaced $ parseChar c

eval :: Expr -> Float
eval e = case e of
    Fail      -> 0
    Add a b   -> eval a + eval b
    Sub a b   -> eval a - eval b
    Mul a b   -> eval a * eval b
    Div a b   -> eval a / eval b
    Num n     -> n

parseExpr::Parser Expr
parseExpr = additive
    where
        additive    = applyOp Add '+' multitive <|> applyOp Sub '-' multitive <|> multitive
        multitive   = applyOp Mul '*' factor <|> applyOp Div '/' factor <|> factor
        factor      = primary <|> parseNum
        primary     = parseSpacedChar '(' *> parseExpr <* parseSpacedChar ')'
        applyOp c o p = c <$> p <*> (parseSpacedChar o *> p)

evalExpr :: String -> Either Error Float
evalExpr s = eval <$> fct s
    where
        fct s = case runParser parseExpr s of
            Right (a, [])    -> Right a
            Right (a, xs)    -> Left $ "Parsing Failed: {Left: " ++ (show xs) ++ "}"
            Left msg         -> Left msg
