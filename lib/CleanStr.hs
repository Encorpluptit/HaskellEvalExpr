module CleanStr where

import Boostrap

isOp :: Char -> Bool
isOp c = elem c "+*/^"

cleanStr :: String->String
cleanStr s@(x:xs)
    | isOp x = [x] ++ "(" ++ cleanStr xs ++ ")"
    | otherwise = fct s
        where
            fct as = case runParser parseFloat as of
                Right (r, rs) -> show r ++ cleanStr rs
                Left msg -> cleanStr as

cleanStr [] = []
