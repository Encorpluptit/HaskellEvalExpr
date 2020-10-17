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
          deriving (Show, Eq)


parseNum :: Parser Expr
--parseNum =  (Number <$> parseFloat)
parseNum = parseChar '+' *> (Number <$> parseFloat) <|> (Number <$> parseFloat)


eval :: Expr -> Float
eval e = case e of
  Add a b -> eval a + eval b
  Sub a b -> eval a - eval b
  Mul a b -> eval a * eval b
  Div a b -> eval a / eval b
  Number n   -> n
  Fail s -> error s


additive :: Parser Expr
--additive = op Add '+' multiply additive <|> op Sub '-' multiply additive <|> multiply
additive = op Add '+' multiply additive <|> op Sub '-' multiply additive <|> multiply

multiply :: Parser Expr
multiply = op Mul '*' factor multiply <|> op Div '/' factor multiply <|> factor

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
--chainLeftAssociative' :: Parser a -> (a -> a -> a) -> Parser a
--chainLeftAssociative' p op = do x <- p; fct x
--    where
--        fct x = do f <- op
--                   b <- p
--                   fct (f x b)
--                <|> return x


op :: (Expr -> Expr -> Expr) -> Char -> Parser Expr -> Parser Expr -> Parser Expr
op c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)

run :: Parser a -> String -> Maybe a
run (Parser p) str = case p str of
    Just (a, "") -> trace (show (evalExpr str)) Just a
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
    where
--        add         = op Add '+' mul add <|> (mul >>= (\x -> neg x)) <|> mul
--        add         = op Add '+' mul add <|> op Sub '-' mul add <|> mul
        add         = op Add '+' mul add <|> neg (neg (neg mul)) <|> mul
        mul         = op Mul '*' factor mul <|> op Div '/' factor mul <|> factor
        factor      = parens <|> parseNum
        parens      = parseSpacedChar '(' *> expr <* parseSpacedChar ')'
        op c o p1 p2 = c <$> p1 <*> (parseSpacedChar o *> p2)
        neg a = Sub <$> a <*> (parseSpacedChar '-' *> add)

evalExpr :: String -> Maybe Float
evalExpr s = eval <$> case runParser expr s of
    Just (a, xs) -> Just a
    Nothing -> Nothing
