module RecursiveDescent where

import Control.Applicative
import Debug.Trace
--import BoostrapJust (
--    Parser,
--    parseChar,
----    parseFloat,
----    parseUFloat,
--    runParser,
--    parseInt)
import Boostrap (
    Parser(..),
    Result,
    Error,
    parseSpaced,
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
            | Fail
            deriving (Show, Eq)

parseAdd::Parser Expr
parseAdd = parseNum

parseNum::Parser Expr
parseNum = Num <$> parseFloat

eval :: Expr -> Float
eval e = case e of
  Fail      -> 0
  Add a b   -> eval a + eval b
  Sub a b   -> eval a - eval b
  Mul a b   -> eval a * eval b
  Div a b -> eval a / eval b
--  Div a b   -> eval a `div` eval b
  Num n     -> n

parseExpr::Parser Expr
parseExpr = additive
    where
--        additive = binOp Add '+' parseNum <|> parseNum
        additive = binOp Add '+' multitive <|> binOp Sub '-' multitive <|> multitive
--        additive    = binOp Add '+' multitive <|> binOp Sub '-' multitive <|> multitive
        multitive   = binOp Mul '*' factor <|> binOp Div '/' factor <|> factor
--        factor      = trace "hello" $ parens <|> parseNum
--        factor      = trace "hello" $ parens <|> parseNum
        factor      = parenthesis <|> parseNum
--        num         = trace "num" parseNum
        parenthesis = parseChar '(' *> parseExpr <* parseChar ')'
        binOp c o p = c <$> p <*> (parseChar o *> p)

evalExpr :: String -> Either Error Float
--evalExpr s = eval <$> runParser parseExpr s
evalExpr s = eval <$> fct s
    where
        fct s = case runParser parseExpr s of
            Right (a, xs)    -> Right a
            Left msg         -> Left msg


--parseAdd::Parser Expr
--parseAdd = parseNum
--
--parseNum::Parser Expr
--parseNum = Num <$> parseFloat
--
--eval :: Expr -> Result Float
--eval e = case e of
--
--  Add a b -> eval a + eval b
--  Sub a b -> eval a - eval b
--  Mul a b -> eval a * eval b
--  Div a b -> eval a / eval b
----  Num n   -> Parser n
--
--parseExpr::Parser Expr
--parseExpr = additive
--    where
--        additive    = binOp Add '+' multitive <|> binOp Sub '-' multitive <|> multitive
--        multitive   = binOp Mul '*' factor <|> binOp Div '/' factor <|> factor
--        factor      = parens <|> lit
--        lit         = parseNum
--        parens      = parseChar '(' *> parseExpr <* parseChar ')'
--        binOp c o p = c <$> p <*> (parseChar o *> p)
--
--evalExpr :: String -> Float
--evalExpr s = eval <$> runParser parseExpr s

--parseExpr :: Parser Expr
--parseExpr = additive
--    where
--        additive    = fail
----        additive    = binOp Add '+' additive <|> fail
----        additive    = binOp Add '+' additive <|> binOp Sub '-' additive <|> fail
--        fail        = Parser (\x -> Left "NoParse")
----        factor      = parenthesis <|> parseNum
----        factor      = (Num <$> parseFloat) <|> fail
----        factor      = (Num <$> parseFloat)
--        binOp c o p = c <$> p <*> (parseChar o *> p)
--
--eval :: Expr -> Float
--eval e = case e of
--  Add a b -> eval a + eval b
--  Sub a b -> eval a - eval b
--  Mul a b -> eval a * eval b
--  Div a b -> eval a / eval b
--  Num n   -> n
--
--evalExpr :: String -> Either String Float
----evalExpr s = eval <$> runParser parseExpr s
--evalExpr s = eval <$> fct
--    where
--        fct = case runParser parseExpr s of
--            Right (x, xs)   -> Right x
--            Left _          -> Left "failed here"
