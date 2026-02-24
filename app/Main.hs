-- app/Main.hs
module Main where

import Ast
import Parser
import PrettyPrinter
import System.Environment (getArgs)
import Text.Parsec (parse)
import TypeChecker

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] -> do
      fd <- readFile name
      case parse parseProgram name fd of
        Left err -> do
          putStrLn "Parsing Error:"
          print err
        Right ast -> do
          putStrLn "AST :"
          print ast
          putStrLn ""
          putStrLn "PrettyPrinter:"
          putStrLn (prettyPrinterProgram ast)
          case checkProgram ast of
            Right _ -> do
              putStrLn "Type Checked!"
            Left err -> do
              putStrLn "Type Rejected! :("
              putStrLn err
    _ -> putStrLn "ERROR: Main -> Please provide a valid file name"
