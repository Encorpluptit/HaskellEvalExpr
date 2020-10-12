module Parser where

import Control.Applicative
import Boostrap (
    Parser,
    Result,
    Error,
    parseChar,
    parseFloat,
    runParser)

data Expr v = Num v Op
                | NoParse

data Op = Op {
}

--data Op = Op {
----    parser      :: Parser Float,
--    additiveParsing    :: String -> Expr Float,
----    additive    :: Expr Float,
--    multitive   :: String -> Expr Float
----    primary     :: Expr Float
----    decimal   :: Result Int
--}
--
evalExpr :: String -> Either Error (Float , String)
evalExpr s = Right (3.0, "lol")
--    where
--        lazyParser = parse

----evalExpr s = case additiveParsing $ parse s of
----    Parser v []     -> Result (v, [])
----    Parser v left   -> Result (v, rem)
----    _ -> error "Parse error"
--
--parse :: String -> Op
--parse s = op
--    where
--        op      = Op add mul
--        add s   = pAdditive s op
--        mul s   = pMultitive s op
--
--
--pAdditive :: String -> Op -> Expr Float
--pAdditive op s = case multitive op s of
--    Parser vleft d' -> case runParser (parseChar '-') s of
--        Parser '+' d'' -> case additiveParsing d'' of
--            Parser vright d''' -> Parser (vleft + vright) d'''
--            _ -> alt2
--        _ -> alt2
--    _ -> alt2
--    where
--        alt2 = Parser 3.0 op
--
--pMultitive :: String -> Op -> Expr Float
--pMultitive d = Parser fct d
--    where
--        fct s = runParser parseFloat s

--pAdditive :: Op -> Expr Float
--pAdditive s = alt1 where
--    -- Additive <- Multitive '+' Additive
--    alt1 = case pMultitive s of
--        Parser vleft s' -> case parseChar s' of
--            ('+':s'') -> case pAdditive s'' of
--                Parser vright s''' -> Parser (vleft + vright) s'''
--                _ -> alt2
--            ('-':s'') -> case pAdditive s'' of
--                Parser vright s''' -> Parser (vleft - vright) s'''
--                _ -> alt2
--            _ -> alt2
--        _ -> alt2
--
--    -- Additive <- Multitive
--    alt2 = case pMultitive s of
--        Parser v s' -> Parser v s'
--        NoParse -> NoParse

