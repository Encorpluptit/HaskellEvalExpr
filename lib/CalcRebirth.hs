module Calc where

import Control.Applicative
import BootstrapJust
import Debug.Trace
import Control.Monad

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Number Float
          | Fail String
          deriving (Show, Eq, Ord)


parseNum :: Parser Expr
--parseNum =  (Number <$> parseFloat)
parseNum = parseChar '+' *> (Number <$> parseFloat) <|> (Number <$> parseFloat)


eval :: Expr -> Float
eval e = case e of
    Add a b     -> eval a + eval b
    Sub a b     -> eval a - eval b
    Mul a b     -> eval a * eval b
    Div a b     -> eval a / eval b
    Number n    -> n
    Fail s      -> error s

start :: Parser Expr
start = Parser fct
    where
        fct s = case runParser factor s of
            Nothing -> Nothing
            Just (x, xs) -> runParser (loop x) xs

loop :: Expr -> Parser Expr
loop a = Parser rp
    where
        rp str = case runParser ((parseSpacedChar '-') *> factor) str of
            Nothing -> Just (a, str)
            Just (x, xs) -> runParser (loop (Sub a x)) xs

--decimal >>= loop

additive :: Parser Expr
--additive = applyOp Add '+' multiply additive <|> applyOp Sub '-' multiply additive <|> multiply
--additive = applyOpAdditive Add '+' multiply additive <|> applyOpAdditive Sub '-' multiply additive <|> multiply
--additive = applyOp Add '+' start multiply <|> applyOp Sub '-' start multiply <|> multiply
--additive = applyOp Add '+' multiply additive <|> applyOp Sub '-' additive (loop (Number 0)) <|> multitive
--additive = applyOp Add '+' multitive additive <|> (Sub <$> start <*> ((parseSpacedChar '-') *> additive)) <|> multitive
--additive = applyOp Add '+' multitive additive <|> start <|> multitive
additive = applyOp Add '+' multitive additive <|> (factor >>= loop) <|> multitive
--additive = applyOp Add '+' multitive additive <|> applyOp Sub '-' multitive additive <|> multitive

multitive :: Parser Expr
multitive = applyOp Mul '*' factor multitive <|> applyOp Div '/' factor multitive <|> factor

factor :: Parser Expr
factor = parenthesis <|> parseNum

parenthesis :: Parser Expr
parenthesis = parseSpacedChar '(' *> expr <* parseSpacedChar ')'

-- GOOD OMFG
--opNeg :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
--opNeg c o p1 p2 = c <$> fct <*> (parseSpacedChar o *> p2)
--    where
--        fct = (p1 >>= (\x -> multiply))


--chainLeftAssociative :: Parser a -> (a -> a -> a) -> a -> Parser a
--chainLeftAssociative p op a = (p `chainLeftAssociative'` op) <|> return a
--
--chainLeftAssociative' :: Parser Expr -> Char -> (Expr -> Expr -> Expr) -> Parser Expr
--chainLeftAssociative' p o op = fct p ((parseSpacedChar o *> additive))
--    where
--        fct x p = do
--            b <- op <$> p <*> (parseSpacedChar o *> additive)
--            fct b p
--            <|> return x

--chainLeftAssociative' :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
--chainLeftAssociative' p op = do x <- p; fct x
--    where
--        fct x = do f <- op
--                   b <- p
--                   fct (f x b)
--                <|> return x
--chainLeftAssociative' :: Parser Expr -> Parser Expr -> (Expr -> Expr -> Expr) -> Parser Expr
----chainLeftAssociative' p1 op = (chainLeftAssociative' p op) <|> p
--chainLeftAssociative' p1 op = (>>=) <$> p1 <*> p1 <|> p1
--chainLeftAssociative' p op = do x <- p; fct x
--    where
--        fct x = do
--                fct p
--                <|> return x


--decimal >>= loop

--chainLeftAssociative :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
----chainLeftAssociative :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
----chainLeftAssociative p op a = chainLeftAssociative'  <|> return a
--chainLeftAssociative p op a = chainLeftAssociative' p op <|> do
--    b <- p
--    return b
--    where f = Parser

--chainLeftAssociative' :: Parser Expr -> Char -> (Expr -> Expr -> Expr) -> Parser Expr
--chainLeftAssociative' p o op = do x <- p; fct x
--    where
--        fct x = do
--            b <- trace "lol" op <$> p <*> (parseSpacedChar o *> additive)
--            fct b
--            <|> return x

--op :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
--op c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)

--op :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
----op c o p1 p2 = c <$> (chainLeftAssociative' p1 o c) <*> (parseSpacedChar o *> p2)
----op c o p1 p2 = c <$> (chainLeftAssociative p1 p' (c <$> p1 <*> p2)) <*> (parseSpacedChar o *> p2)
--op c o p1 p2 = chainLeftAssociative p1 p' (c <$> p1 <*> (parseSpacedChar o *> p2)) <|> c <$> p1 <*> (parseSpacedChar o *> p2)
--    where
--        p' = Parser (c)
--        fct s = runParser (c <$> p1 <*> (parseSpacedChar o *> p2)) s
----        fct a b = do
----            parseSpacedChar o
----            return c

--decimal >>= loop

--op :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
--applyOpAdditive :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
--applyOpAdditive op c p1 p2 = chainLeftAssociative' p' op <|> p'
--    where
--        p' = op <$> p1 <*> (parseSpacedChar c *> p2)
--
applyOp :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
applyOp op c p1 p2 = op <$> p1 <*> (parseSpacedChar c *> p2)
--        p' = op <$> p1 <*> p2

--applyOp c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)
--op c o p1 p2 = (chainLeftAssociative' p2 o c)
--    <|> (c <$> p1 <*> (parseSpacedChar o *> p2))

run :: Parser a -> String -> Maybe a
run (Parser p) str = case p str of
    Just (a, "") -> Just a
    _            -> Nothing


--expr :: Parser Integer
--expr = do t <- term; cont t
--  where
--  cont t1 = do op <- addop
--               t2 <- term
--               cont (t1 `op` t2)
--            <|> return t1

--parseMany :: Parser a -> Parser [a]
--parseMany parser = Parser fct
--    where
--        fct s = case runParser ((:) <$> parser <*> parseMany parser) s of
--            Nothing -> Just ([], s)
--            r       -> r

--parseNegExpr :: Parser Expr -> Parser Expr ->Parser Expr
--parseNegExpr x  = return x >>= (+2)
--parseNegExpr :: Parser Expr -> Parser Expr
--parseNegExpr parser = Parser fct
--      where
--          fct s = case runParser (msum ((:) <$> parser <*> parseMany parser)) s of
--              Nothing -> Just (0, s)
--              r       -> r

--routine2 :: Parser Expr -> Parser Expr
--routine2 x = Parser fct
--    where
--        fct s = case trace (showPole x) (landLeft 1 x) of
--        case trace (showPole x) (landLeft 1 x) of
--            Just (a, b) -> return (a, b) >>= routine2
--            Nothing -> Just x
--
--parseNegExpr :: Parser Expr
--parseNegExpr = Parser fct
--    where
--        fct s = case runParser (op Sub '-' multiply additive) s of
--             Nothing -> return
----             Nothing -> Parser $ const Nothing
--             Just (a, as) -> parseNegExpr

expr :: Parser Expr
expr = additive
--    where
----        add         = op Add '+' mul add <|> (mul >>= (\x -> neg x)) <|> mul
----        add         = op Add '+' mul add <|> op Sub '-' mul add <|> mul
--        add         = op Add '+' mul add <|> neg (neg (neg mul)) <|> mul
--        mul         = op Mul '*' factor mul <|> op Div '/' factor mul <|> factor
--        factor      = parens <|> parseNum
--        parens      = parseSpacedChar '(' *> expr <* parseSpacedChar ')'
--        op c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)
--        neg a = Sub <$> a <*> (parseSpacedChar '-' *> add)

evalExpr :: String -> Maybe Float
evalExpr s = eval <$> case runParser expr s of
    Just (a, xs) -> Just a
    Nothing -> Nothing
