module CleanStr where

import Boostrap

isOp :: Char -> Bool
isOp c = elem c "+-*/^"

isPar :: Char -> Bool
isPar c = elem c "()"

cleanStr :: String->String
cleanStr s@(x:xs)
--    | isPar x   = [x] ++ cleanStr xs
    | isOp x    = [x] ++ "(" ++ cleanStr xs ++ ")"
    | otherwise = fct s
        where
            fct str@(a:as) = case runParser parseFloat str of
                Right (r, rs) ->  show r ++ cleanStr rs
                Left msg -> cleanStr as

cleanStr [] = []
