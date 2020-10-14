module RecursiveDescent where

import Control.Applicative
import Boostrap (
    Parser,
    Result,
    Error,
    parseChar,
    parseFloat,
    parseUFloat,
    runParser,
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
            deriving (Show, Eq)

parseAdd::Parser Expr
parseAdd = parseNum
--
--parseAdd::Parser Expr
--parseAdd
--

parseNum::Parser Expr
parseNum = Num <$> parseFloat
--
--parseExpr::Parser Expr
--parseExpr = parseNum

eval :: Expr -> Parser Float
eval e = case e of
  Add a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Mul a b -> eval a * eval b
  Div a b -> eval a / eval b
--  Num n   -> Parser n

parseExpr::Parser Expr
parseExpr = additive
    where
        additive    = binOp Add '+' multitive <|> binOp Sub '-' multitive <|> multitive
        multitive   = binOp Mul '*' factor <|> binOp Div '/' factor <|> factor
        factor      = parens <|> lit
        lit         = parseNum
        parens      = parseChar '(' *> parseExpr <* parseChar ')'
        binOp c o p = c <$> p <*> (parseChar o *> p)

evalExpr :: String -> Result Float
evalExpr s = eval <$> runParser parseExpr s

--evalExpr :: String -> Result Float
--evalExpr s = case additive s of
--    Num v rem   -> Right (v, rem)
--    _           -> error "Parse error"

