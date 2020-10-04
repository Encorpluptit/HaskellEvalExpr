module Main where

import System.Environment (getArgs)
import System.Exit
import System.IO
import BasicTree


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
    [] ->  do
        exit_error "No args given"
    (x:_) -> case exec x of
         Left msg -> do
           exit_error $ "Error: " ++ msg
         Right (v, _) -> do
           print v

