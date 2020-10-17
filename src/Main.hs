module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Printf
import MonadicParser

writeError :: String -> IO ()
writeError str = hPutStrLn stderr ("Error : " ++ str)

exitError :: String -> IO a
exitError [] = exitWith $ ExitFailure 84
exitError str = do
    writeError str
    exitWith  $ ExitFailure 84

rounded :: Float -> Int -> Float
rounded f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

main :: IO()
main = do
    args <- getArgs
    case args of
        [] ->  exitError "No args given"
        (x:_) -> case evalExpr x of
            Just a      -> printf "%.2f\n" (rounded a 3)
            Nothing     -> exitError "Parsing Failed"
