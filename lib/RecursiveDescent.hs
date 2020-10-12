module RecursiveDescent where

import Control.Applicative
import Boostrap (
    Parser,
    Result,
    Error,
    parseChar,
    parseFloat,
    runParser,
    parseInt)

-- Recursive Descent
--
-- https://www.booleanworld.com/building-recursive-descent-parsers-definitive-guide/

-- Additive and Multiplicative Principles:
--
-- http://www.math.wichita.edu/~hammond/class-notes/section-counting-basics.html
-- https://courses.lumenlearning.com/ivytech-collegealgebra/chapter/using-the-addition-and-multiplication-principles/

data Expr x = Num x String
            | Error

evalExpr :: String -> Result Int
evalExpr s = case pAdditive s of
		Num v rem -> Right (v, rem)
		_ -> error "Parse error"

-- additive-precedence {+ -}
pAdditive :: String -> Expr Int
pAdditive s = alt1 where

    -- Additive <- Multitive '+' Additive
    alt1 = case pMultitive s of
        Num vleft s' -> case s' of
            ('+':s'') -> case pAdditive s'' of
                Num vright s''' -> Num (vleft + vright) s'''
                _ -> alt2
            ('-':s'') -> case pAdditive s'' of
                Num vright s''' -> Num (vleft - vright) s'''
                _ -> alt2
            _ -> alt2
        _ -> alt2

    -- Additive <- Multitive
    alt2 = case pMultitive s of
	     Num v s' -> Num v s'
	     Error -> Error

-- multiplicative-precedence {}
pMultitive :: String -> Expr Int
-- Multitive <- Primary '*' Multitive
pMultitive s = alt1 where

    alt1 = case pPrimary s of
        Num vleft s' -> case s' of
            ('*':s'') -> case pMultitive s'' of
                Num vright s''' -> Num (vleft * vright) s'''
                _ -> alt2
            _ -> alt2
        _ -> alt2

    -- Multitive <- Primary
    alt2 = case pPrimary s of
        Num v s'    -> Num v s'
        Error       -> Error

-- Parse a primary expression
pPrimary :: String -> Expr Int
-- Primary <- '(' Additive ')'
pPrimary s = alt1 where

    alt1 = case s of
        ('(':s') -> case pAdditive s' of
            Num v s'' -> case s'' of
                (')':s''') -> Num v s'''
                _ -> alt2
            _ -> alt2
        _ -> alt2

    -- Primary <- Decimal
    alt2 = case runParser parseInt s of
        Right (a, as)    -> Num a as
        Left msg         -> Error
