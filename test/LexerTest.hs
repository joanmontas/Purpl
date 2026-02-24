-- test/LexerTest.hs

import Lexer
import Test.Hspec


main :: IO ()
main = hspec $ do

    describe "Tokenize Single Non-builtin Identifier No Error" $ do
        it "should lex '' into []" $ do
            let input = ""
            let tokens = tokenize input
            let expectedOutput = []
            case tokens of
                Left err -> expectationFailure (show err)
                Right t -> t `shouldBe` expectedOutput

        it "should lex 'Hello_World' into Identifier Hello_World" $ do
            let input = "Hello_World"
            let tokens = tokenize input
            let expectedOutput = [Identifier "Hello_World"]
            case tokens of
                Left err -> expectationFailure (show err)
                Right t -> t `shouldBe` expectedOutput

        it "should lex 'LavenderTime' into Identifier LavenderTime" $ do
            let input = "LavenderTime"
            let tokens = tokenize input
            let expectedOutput = [Identifier "LavenderTime"]
            case tokens of
                Left err -> expectationFailure (show err)
                Right t -> t `shouldBe` expectedOutput

        it "should lex 'kljdfg_lkdfg345' into Identifier kljdfg_lkdfg345" $ do
            let input = "kljdfg_lkdfg345"
            let tokens = tokenize input
            let expectedOutput = [Identifier "kljdfg_lkdfg345"]
            case tokens of
                Left err -> expectationFailure (show err)
                Right t -> t `shouldBe` expectedOutput

    -- describe "Tokenize Single Non-builtin Identifier With Error" $ do
    --     it "should lex 'this_should_*_not_exist' into Identifier Hello_World" $ do
    --         let input = "this_should_*_not_exist"
    --         let tokens = tokenize input
    --         let expectedOutput = [Identifier "this_should_*_not_exist"]
    --         case tokens of
    --             Left err -> expectationFailure (show err)
    --             Right t -> t `shouldBe` expectedOutput

    describe "Tokenize Class Definition With No Error" $ do
        it "should lex 'class A extends Object { A() { super(); } }' into []" $ do
            let input = "class A extends Object { A() { super(); } }"
            let tokens = tokenize input
            let expectedOutput = [IdentifierKeyword "class",Identifier "A",IdentifierKeyword "extends",IdentifierKeyword "Object",SymbolKeyword "{",Identifier "A",SymbolKeyword "(",SymbolKeyword ")",SymbolKeyword "{",IdentifierKeyword "super",SymbolKeyword "(",SymbolKeyword ")",SymbolKeyword ";",SymbolKeyword "}",SymbolKeyword "}"]
            case tokens of
                Left err -> expectationFailure (show err)
                Right t -> t `shouldBe` expectedOutput