module Main where

import System.Environment (getArgs)
import Interpreter
import Parser

{-
  Run a single file.
-}
runFromFile :: String -> IO ()
runFromFile filenm = do
  pe <- parseFromFile filenm
  case pe of Right p -> prog p
             Left  e -> putStrLn e     

{-
  Run all files given in command line.
-}
main :: IO ()
main = do
  args <- getArgs
  mapM_ runFromFile args
