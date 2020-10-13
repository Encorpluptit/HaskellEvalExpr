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

data Expr x = Num x String
            | Error
            deriving (Show, Eq)

evalExpr :: String -> Result Float
evalExpr s = case additive s of
    Num v rem   -> Right (v, rem)
    _           -> error "Parse error"

-- additive-precedence {+ -}
additive :: String -> Expr Float
-- Additive <- Multitive '+':'-' Additive
additive s = case multitive s of
    Num leftValue s' -> case s' of
        ('+':s'') -> case additive s'' of
            Num rightValue s''' -> Num (leftValue + rightValue) s'''
            _                   -> failed
        ('-':s'') -> case additive s'' of
            Num rightValue s''' -> Num (leftValue - rightValue) s'''
            _                   -> failed
        _ -> failed
    _ -> failed

    where
        -- Additive <- Multitive
        failed = case multitive s of
           Num v s' -> Num v s'
           Error -> Error

-- multiplicative-precedence {* /}
multitive :: String -> Expr Float
-- Multitive <- Primary '*':'/' Multitive
multitive s = case primary s of
    Num leftValue s' -> case s' of
        ('*':s'') -> case multitive s'' of
            Num rightValue s''' -> Num (leftValue * rightValue) s'''
            _                   -> failed
        ('/':s'') -> case multitive s'' of
            Num rightValue s''' -> Num (leftValue / rightValue) s'''
            _                   -> failed
        _ -> failed
    _ -> failed

    where
        -- Multitive <- Primary
        failed = case primary s of
            Num v s'    -> Num v s'
            Error       -> Error

-- Parse a primary expression
primary :: String -> Expr Float
-- Primary <- '(' Additive ')'
primary s = case s of
    ('(':s') -> case additive s' of
        Num v s'' -> case s'' of
            (')':s''')  -> Num v s'''
            _           -> failed
        _ -> failed
    _ -> failed

    where
        -- Primary <- Decimal
        failed = case runParser parseFloat s of
            Right (a, as)    -> Num a as
            Left msg         -> Error
