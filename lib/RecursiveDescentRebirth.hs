module RecursiveDescentRebirth where

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
--parseNum =  (Number <$> parseFloat)
parseNum = parseChar '+' *> (Number <$> parseFloat) <|> (Number <$> parseFloat)

eval :: Expr -> Either String Float
eval e = case e of
    Add a b   -> fct a b (+)
    Sub a b   -> fct b a (-)
    Mul a b   -> fct a b (*)
    Div a b   -> fct a b (/)
    Number n  -> Right n
    Fail      -> Left "LOOOOOOOL"
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
        additive    = applyOp Sub '-' multitive additive <|> applyOp Add '+' multitive additive <|> multitive
        multitive   = applyOp Mul '*' factor additive <|> applyOp Div '/' factor multitive <|> factor
        factor      = parseNum <|> primary
        primary     = parseSpacedChar '(' *> parseExpr <* parseSpacedChar ')'
        applyOp c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)

evalExpr :: String -> Either Error Float
evalExpr s = case eval <$> fct s of
        Right a     -> a
        Left msg    -> Left msg
    where
        fct a = case runParser parseExpr s of
            Right (a, [])    -> Right a
            Right (a, xs)    -> Left $ "Parsing Failed: {Left: " ++ (show xs) ++ "}"
            Left msg         -> Left msg
