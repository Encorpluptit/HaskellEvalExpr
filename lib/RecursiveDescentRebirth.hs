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

data Expr = Number Float
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Power Expr Expr
            | Fail
            deriving (Show, Eq, Ord)

parseNum :: Parser Expr
parseNum = parseChar '+' *> (Number <$> parseFloat) <|> (Number <$> parseFloat)

eval :: Expr -> Either String Float
eval e = case e of
    Fail      -> Left "LOOOOOOOL"
    Add a b   -> fct a b (+)
    Sub a b   -> fct a b (-)
    Mul a b   -> fct a b (*)
    Div a b   -> fct a b (/)
    Number n  -> Right n
    where
        fct a b op = case eval a of
            Right x -> case eval b of
                Right y     -> Right $ op x y
                Left msg    -> Left msg
            Left msg -> Left msg

parseSpacedChar :: Char -> Parser Char
parseSpacedChar c = parseSpaced $ parseChar c

parseExpr::Parser Expr
parseExpr = additive
    where
        additive    = applyOp Add '+' multitive <|> applyOp Sub '-' multitive <|> multitive
        multitive   = applyOp Mul '*' factor <|> applyOp Div '/' factor <|> factor
        factor      = primary <|> parseNum
        primary     = parseSpacedChar '(' *> parseExpr <* parseSpacedChar ')'
        applyOp c o p = c <$> p <*> (parseSpacedChar o *> p)

evalExpr :: String -> Either Error Float
--evalExpr s = eval <$> runParser parseExpr s
evalExpr s = case eval <$> fct s of
        Right a     -> a
        Left msg    -> Left msg
    where
        fct a = case runParser parseExpr s of
            Right (a, [])    -> Right a
            Right (a, xs)    -> Left $ "Parsing Failed: {Left: " ++ (show xs) ++ "}"
            Left msg         -> Left msg
