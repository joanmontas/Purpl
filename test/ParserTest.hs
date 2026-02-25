module Main (main) where

import Ast
import Data.Either (isLeft, isRight)
import Parser hiding (main)
import Test.Hspec
import Text.Parsec

main :: IO ()
main = hspec $ do
  describe "parseValue should parse built in booleans and integers" $ do
    it "parseValue should parse Bool true" $ do
      let input = "true"
      let b = parse parseValue "" input
      b `shouldBe` (Right TrueValue)

    it "parseValue should parse Bool false" $ do
      let input = "false"
      let b = parse parseValue "" input
      b `shouldBe` (Right FalseValue)

    it "parseValue should parse int 0" $ do
      let input = show 0
      let i = parse parseValue "" input
      i `shouldBe` (Right (IntegerValue 0))

    it "parseValue should parse int 1" $ do
      let input = show 1
      let i = parse parseValue "" input
      i `shouldBe` (Right (IntegerValue 1))

    it "parseValue should not parse June" $ do
      let input = "June"
      let i = parse parseValue "" input
      i `shouldSatisfy` isLeft

  describe "ParseGroundType should parse all defined ground types" $ do
    it "parseGroundType should parse Bool" $ do
      let input = "bool"
      let gt = parse parseGroundType "" input
      gt `shouldBe` (Right BoolGroundType)

    it "parseGroundType should parse Int" $ do
      let input = "int"
      let i = parse parseGroundType "" input
      i `shouldBe` (Right IntGroundType)

    it "parseGroundType should parse Identifier representing class" $ do
      let input = "This_is_my_class_name"
      let i = parse parseGroundType "" input
      i `shouldBe` Right (ClassGroundType "This_is_my_class_name")

  describe "ParseType" $ do
    it "Should pass simple int type definitions with out purpose" $ do
      let input = "int {||}"
      let ps = parse parseType "" input
      ps `shouldBe` Right (Type {gtType = IntGroundType, piType = PurposeSet {purposes = [], rho = Nothing}})

    it "Should pass simple int type definitions with single purpose" $ do
      let input = "int {|p0|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = IntGroundType, piType = PurposeSet {purposes = ["p0"], rho = Nothing}})

    it "Should pass simple int type definitions with two purpose" $ do
      let input = "int {|p0, p1|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = IntGroundType, piType = PurposeSet {purposes = ["p0", "p1"], rho = Nothing}})

    it "Should pass simple int type definitions with three purpose" $ do
      let input = "int {|p0, p1, p2|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = IntGroundType, piType = PurposeSet {purposes = ["p0", "p1", "p2"], rho = Nothing}})

    it "Should pass simple int type definitions with out purpose" $ do
      let input = "int {||}"
      let ps = parse parseType "" input
      ps `shouldBe` Right (Type {gtType = IntGroundType, piType = PurposeSet {purposes = [], rho = Nothing}})

    it "Should pass simple class type definitions with single purpose" $ do
      let input = "myclass {|p0|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = ClassGroundType "myclass", piType = PurposeSet {purposes = ["p0"], rho = Nothing}})

    it "Should pass simple int type definitions with two purpose" $ do
      let input = "myclass {|p0, p1|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = ClassGroundType "myclass", piType = PurposeSet {purposes = ["p0", "p1"], rho = Nothing}})

    it "Should pass simple int type definitions with three purpose" $ do
      let input = "myclass {|p0, p1, p2|}"
      let gt = parse parseType "" input
      gt `shouldBe` Right (Type {gtType = ClassGroundType "myclass", piType = PurposeSet {purposes = ["p0", "p1", "p2"], rho = Nothing}})

  describe "parsePurposeSet should parse list of purpose" $ do
    it "parsePurposeSet should parse empty set {||}" $ do
      let input = "{||}"
      let ps = parse parsePurposeSet "" input
      ps `shouldBe` Right (PurposeSet {purposes = [], rho = Nothing})

    it "parsePurposeSet should parse singe element set {|p0|}" $ do
      let input = "{|p0|}"
      let ps = parse parsePurposeSet "" input
      ps `shouldBe` Right (PurposeSet {purposes = ["p0"], rho = Nothing})

    it "parsePurposeSet should parse double element set {|p0, p1|}" $ do
      let input = "{|p0, p1|}"
      let ps = parse parsePurposeSet "" input
      ps `shouldBe` Right (PurposeSet {purposes = ["p0", "p1"], rho = Nothing})

    it "parsePurposeSet should parse triple element set {|p0, p1, p2|}" $ do
      let input = "{|p0, p1, p2|}"
      let ps = parse parsePurposeSet "" input
      ps `shouldBe` Right (PurposeSet {purposes = ["p0", "p1", "p2"], rho = Nothing})

    it "parsePurposeSet should parse empty element set with a Rho {|| RHO|}" $ do
      let input = "{|| Rho|}"
      let ps = parse parsePurposeSet "" input
      ps `shouldBe` Right (PurposeSet {purposes = [], rho = Just "Rho"})

  describe "parseTerm is able to parse a variety of terms: var" $ do
    it "should be able to parse simple variable" $ do
      let input = "x"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (VarTerm input)

    it "should be able to parse simple variable" $ do
      let input = "y"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (VarTerm input)

    it "should be able to parse complex variable" $ do
      let input = "myvar1_23"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (VarTerm input)

    it "should NOT be able to parse variables starting with numbers" $ do
      let input = "123myvar1_23"
      let ps = parse parseTerm "" input
      ps `shouldNotBe` Right (VarTerm input)

  describe "parseTerm is able to parse a variety of terms: Field Access" $ do
    it "should be able to parse simple field access" $ do
      let input = "x.f"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (FieldAccessTerm {sTerm = VarTerm "x", fTerm = "f"})

    it "should be able to parse simple field access" $ do
      let input = "y.f"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (FieldAccessTerm {sTerm = VarTerm "y", fTerm = "f"})

    it "should be able to parse complex field access" $ do
      let input = "myvar1_23.f"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (FieldAccessTerm {sTerm = VarTerm "myvar1_23", fTerm = "f"})

    it "should be able to parse recursive field access" $ do
      let input = "x.y.b.c.d"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (FieldAccessTerm {sTerm = FieldAccessTerm {sTerm = FieldAccessTerm {sTerm = FieldAccessTerm {sTerm = VarTerm "x", fTerm = "y"}, fTerm = "b"}, fTerm = "c"}, fTerm = "d"})

  describe "parseTerm should be able to parse a variety of terms: Method Call Term" $ do
    it "should be able to parse simple field access" $ do
      let input = "foo.bar(x)"
      let ps = parse parseTerm "" input
      ps `shouldBe` Right (MethodCallTerm {sTerm = VarTerm "foo", mTerm = "bar", argTerm = [VarTerm "x"]})

  describe "parseArgumentDecl" $ do
    it "parseArgumentDecl should pass int with empty purpose without arrow 'a : int {||}'" $ do
      let input = "a : int {||}"
      let ps = parse parseArgumentDecl "" input
      ps
        `shouldBe` Right
          ( ArgumentDecl
              { nArgumentDecl = "a",
                tArgumentDecl =
                  Type
                    { gtType = IntGroundType,
                      piType = PurposeSet {purposes = [], rho = Nothing}
                    },
                piArgumentDecl = PurposeSet {purposes = [], rho = Nothing}
              }
          )

    it "parseArgumentDecl should pass int with empty purpose with empty arrow 'a : int {||} => {||}'" $ do
      let input = "a : int {||} => {||}"
      let ps = parse parseArgumentDecl "" input
      ps
        `shouldBe` Right
          ( ArgumentDecl
              { nArgumentDecl = "a",
                tArgumentDecl =
                  Type
                    { gtType = IntGroundType,
                      piType = PurposeSet {purposes = [], rho = Nothing}
                    },
                piArgumentDecl = PurposeSet {purposes = [], rho = Nothing}
              }
          )

    it "parseArgumentDecl should pass int with purpose and with empty arrow 'a : int {|p0|} => {||}'" $ do
      let input = "a : int {|p0|} => {||}"
      let ps = parse parseArgumentDecl "" input
      ps
        `shouldBe` Right
          ( ArgumentDecl
              { nArgumentDecl = "a",
                tArgumentDecl =
                  Type
                    { gtType = IntGroundType,
                      piType = PurposeSet {purposes = ["p0"], rho = Nothing}
                    },
                piArgumentDecl = PurposeSet {purposes = [], rho = Nothing}
              }
          )

    it "parseArgumentDecl should pass int with purpose and with single purpose arrow 'a : int {|p0|} => {|p1|}'" $ do
      let input = "a : int {|p0|} => {|p1|}"
      let ps = parse parseArgumentDecl "" input
      ps
        `shouldBe` Right
          ( ArgumentDecl
              { nArgumentDecl = "a",
                tArgumentDecl =
                  Type
                    { gtType = IntGroundType,
                      piType = PurposeSet {purposes = ["p0"], rho = Nothing}
                    },
                piArgumentDecl = PurposeSet {purposes = ["p1"], rho = Nothing}
              }
          )

  describe "parseClassDecl" $ do
    it "should be able to parse a simple class" $ do
      let input = "class foo extends bar { int a; function foo(a : int {|p0|} => {|p1|} ){ a := a; } function aGetter() -> int {|p3|} { return a; } }"
      let ps = parse parseClassDecl "" input

      ps
        `shouldBe` Right
          ( ClassDecl
              { cClassDecl = "foo",
                extendsClassDecl = "bar",
                gtClassDecl = [GroundField {gGroundField = IntGroundType, fGroundField = "a"}],
                kClassDecl =
                  ConstructorDecl
                    { mConstructorDecl = "foo",
                      ptxpiConstructorDecl =
                        [ ArgumentDecl
                            { nArgumentDecl = "a",
                              tArgumentDecl = Type {gtType = IntGroundType, piType = PurposeSet {purposes = ["p0"], rho = Nothing}},
                              piArgumentDecl = PurposeSet {purposes = ["p1"], rho = Nothing}
                            }
                        ],
                      sConstructorDecl =
                        BlockStatements
                          { bStatements =
                              [XAssignmentStatements {xStatements = "a", tStatements = VarTerm "a"}]
                          }
                    },
                mClassDecl =
                  [ MethodDecl
                      { tMethodDecl = Type {gtType = IntGroundType, piType = PurposeSet {purposes = ["p3"], rho = Nothing}},
                        mMethodDecl = "aGetter",
                        ptxpiMethodDecl = [],
                        sMethodDecl =
                          BlockStatements
                            { bStatements = [ReturnStatements {tStatements = VarTerm "a"}]
                            },
                        sPurposeState = Nothing,
                        fPurposeState = Nothing
                      }
                  ]
              }
          )
