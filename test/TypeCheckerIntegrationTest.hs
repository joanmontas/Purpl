module Main (main) where

import Ast
import Data.Either
import Data.Map qualified as Map
import Parser hiding (main)
import Test.Hspec
import Text.Parsec
import TypeChecker hiding (main)
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import System.Directory (listDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import System.Directory (listDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_)

integrate :: FilePath -> Bool -> IO Bool
integrate name accepted_or_rejected = do
    fd <- readFile name
    case parse parseProgram name fd of
        Left err -> do
            putStrLn "Parsing Error:"
            print err
            return False
        Right ast -> do
            -- putStrLn "AST :"
            -- print ast
            -- putStrLn ""
            -- putStrLn "PrettyPrinter:"
            -- putStrLn (prettyPrinterProgram ast)
            case checkProgram ast of
                Right _ -> do
                    if accepted_or_rejected
                        then putStrLn "Type Checked!" >> return True
                        else putStrLn "ERROR: Accepted when should have been rejected." >> return False

                Left err -> do
                    if accepted_or_rejected
                        then putStrLn ("ERROR: Type Rejected when should have been accepted: " ++ err) >> return False
                        else putStrLn ("Rejected! Good work! " ++ err) >> return True


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "DEBUG: Running from CWD: " ++ cwd
  
  hspec $ do
    describe "Integration Tests: Accepted Cases" $ do
      let dir = "./test/Integration_Tests/Accepted"
      files <- runIO $ listDirectory dir
      let javaFiles = filter (".java" `isSuffixOf`) files
      
      forM_ javaFiles $ \f -> do
        it ("should accept " ++ f) $ do
          res <- integrate (dir </> f) True
          res `shouldBe` True

    describe "Integration Tests: Rejected Cases" $ do
      let dir = "./test/Integration_Tests/Rejected"
      files <- runIO $ listDirectory dir
      let javaFiles = filter (".java" `isSuffixOf`) files
      
      forM_ javaFiles $ \f -> do
        it ("should reject " ++ f) $ do
          res <- integrate (dir </> f) False
          res `shouldBe` True