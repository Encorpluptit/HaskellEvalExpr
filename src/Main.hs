module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import Calc


write_error :: String -> IO ()
write_error str = hPutStrLn stderr ("Error : " ++ str)

exit_error :: String -> IO a
exit_error [] = exitWith $ ExitFailure 84
exit_error str = do
    write_error str
    exitWith  $ ExitFailure 84

main :: IO()
main = do
  args <- getArgs
  case args of
    [] ->  exit_error "No args given"
    (x:_) -> case evalExpr x of
         Nothing -> exit_error $ "Error"
         Just v -> print v

