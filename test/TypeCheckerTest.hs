module Main (main) where

import Ast
import Data.Either
import Data.Map qualified as Map
import Parser hiding (main)
import Test.Hspec
import Text.Parsec
import TypeChecker hiding (main)

-- -- https://hackage.haskell.org/package/hspec-expectations-0.8.4/docs/Test-Hspec-Expectations.html

main :: IO ()
main = hspec $ do
  -- -- Helpers
  -- describe "Γ'' = Γ'1 ⊓ Γ'2" $ do
  --   it "should accept non-conflicting types" $ do
  --     let g1 = Map.fromList [("x", Type IntGroundType (PurposeSet ["Internal", "Private"] Nothing))]
  --     let g2 = Map.fromList [("x", Type IntGroundType (PurposeSet ["Internal"] Nothing))]
  --     let g = intersectionGamma g1 g2
  --     tc `shouldSatisfy` isRight

  describe "TypeChecker should be able to check Terms" $ do
    it "should TypeCheckTerm true" $ do
      let input = "true"
      let ast = parse parseTerm "" input
      case ast of
        Right term -> do
          let tc = checkTerm Map.empty Map.empty [] term
          case tc of
            Right (Type BoolGroundType AnyPurpose, _) ->
              return ()
            Right (Type _ p, _) ->
              expectationFailure ("Expected AnyPurpose, but got: " ++ show p)
            Left err ->
              expectationFailure ("Type checker failed: " ++ err)
        Left err ->
          expectationFailure ("Parser failed: " ++ show err)

    it "should TypeCheckTerm false" $ do
      let input = "false"
      let ast = parse parseTerm "" input
      case ast of
        Right term -> do
          let tc = checkTerm Map.empty Map.empty [] term
          case tc of
            Right (Type BoolGroundType AnyPurpose, _) ->
              return ()
            Right (Type _ p, _) ->
              expectationFailure ("Expected AnyPurpose, but got: " ++ show p)
            Left err ->
              expectationFailure ("Type checker failed: " ++ err)
        Left err ->
          expectationFailure ("Parser failed: " ++ show err)

    it "should TypeCheckTerm number" $ do
      let input = "10611197110"
      let ast = parse parseTerm "" input
      case ast of
        Right term -> do
          let tc = checkTerm Map.empty Map.empty [] term
          case tc of
            Right (Type IntGroundType AnyPurpose, _) ->
              return ()
            Right (Type _ p, _) ->
              expectationFailure ("Expected AnyPurpose, but got: " ++ show p)
            Left err ->
              expectationFailure ("Type checker failed: " ++ err)
        Left err ->
          expectationFailure ("Parser failed: " ++ show err)

    it "should reject LetStatements x wants to be used for Internal purpose, but then is used for Private purpose" $ do
      let input = "let x : int {|Internal|} := 5 in {let y : int {|Internal, Private|} := x in {skip;};}"
      case parse parseLetStatements "" input of
        Right stmt -> do
          let tc = checkStatement Map.empty Map.empty [] stmt
          tc `shouldSatisfy` isLeft
          case tc of
            Left err -> return ()
            Right _ -> 
              expectationFailure "Type checker should have rejected as x can't be used for Private Purpose"
        Left err -> 
          expectationFailure ("Parser failed: " ++ show err)

    it "should accept LetStatements as x wants to be used in Internal and Private Purpose, then it is use for Internal" $ do
      let input = "let x : int {|Internal, Private|} := 5 in {let y : int {|Internal|} := x in {skip;};}"
      case parse parseLetStatements "" input of
        Left err -> expectationFailure ("Parser failed: " ++ show err)
        Right stmt -> do
          let tc = checkStatement Map.empty Map.empty [] stmt
          tc `shouldSatisfy` isRight
          case tc of
            Right gamma' -> 
              gamma' `shouldBe` Map.empty
            Left err -> 
              expectationFailure $ "Should have accepted the flow from {Internal, Private} to {Internal}, but got error: " ++ err


    it "should accept an IfStatement with valid local LetStatements in both branches" $ do
      let input = "if (true) then {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      
      case parse parseIfStatements "" input of
        Left err -> 
          expectationFailure ("Parser failed: " ++ show err)
        
        Right ast -> do
          let result = checkStatement initialClassTable Map.empty [] ast
          
          result `shouldSatisfy` isRight
          
          case result of
            Right finalGamma -> 
              finalGamma `shouldBe` Map.empty
            Left err -> 
              expectationFailure ("Expected success, but got Type/Security Error: " ++ err)

    it "should reject if the 'then' branch contains a purpose escalation (p1 to p0)" $ do
      let input = "if (true) then {let y : int {|p1|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      
      case parse parseIfStatements "" input of
        Left err -> expectationFailure ("Parser failed: " ++ show err)
        Right ast -> do
          let result = checkStatement initialClassTable Map.empty [] ast
          
          result `shouldSatisfy` isLeft


    it "should reject if the 'else' branch contains a purpose escalation (p1 to p0)" $ do
      let input = "if (true) then {let y : int {|p0|} := 123 in {let x : int {| p0 |} := y in {skip;}}} else {let y : int {|p1|} := 123 in {let x : int {| p0 |} := y in {skip;}}}"
      
      case parse parseIfStatements "" input of
        Left err -> expectationFailure ("Parser failed: " ++ show err)
        Right ast -> do
          let result = checkStatement initialClassTable Map.empty [] ast
          
          result `shouldSatisfy` isLeft

  describe "TypeChecker should be able to check Method Declarations" $ do
    it "should reject a MethodDecl where return type (int) does not match signature (bool)" $ do
      let input = unlines 
            [
            "class TestNew extends Object {",
            "  int aIdentity;",
            "  function TestNew(a : int {|Internal|} => {|Internal|}) {",
            "    aIdentity := a;",
            "  }",
            "  function identityTestNewBool(tt : bool {|p0|}) -> bool {|p0|} {",
            "   return 123;",
            "  }",
            "  function identityTestNewBoolInt(tt : bool {|p0|}, i : int {|p1|}) -> bool {|p0|} {",
            "   return tt;",
            "  }",
            "}"
            ]
      case parse parseProgram "" input of
        Right (Program classList) -> do
          let table = classTableConstructor (Program classList)
          case findMethodInAST classList "TestNew" "identityTestNewBool" of
            Just mDecl -> do
              let result = checkMethodDecl table "TestNew" mDecl
              result `shouldSatisfy` isLeft
              case result of
                Left err -> err `shouldContain` "Type Mismatch"
                Right _  -> expectationFailure "Should have caught the int/bool return mismatch"
            Nothing -> expectationFailure "Method search failed"
        Left err -> expectationFailure ("Parser failed: " ++ show err)