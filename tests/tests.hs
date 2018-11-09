module Main where

import Test.Hspec

import HCC.Lexer
import qualified HCC.Token as T

test :: Spec
test = do
    describe "Should return true" $ do
        it "True is True" $ do
            True `shouldBe` True

basicMain :: String
basicMain = "int main(){\n\treturn 5;\n}"

basicParsing :: Spec
basicParsing = do
    describe "A basic main function should compile" $ do
        it "The Lexer should properly tokenize the input" $ do
            HCC.Lexer.lex basicMain `shouldBe` 
                Right [ T.TokenData T.TInt 1 4
                      , T.TokenData (T.TId "main") 1 9
                      , T.TokenData T.TOpenParen 1 10
                      , T.TokenData T.TCloseParen 1 11
                      , T.TokenData T.TOpenBrace 1 12
                      , T.TokenData T.TReturn 2 15
                      , T.TokenData (T.TIntLiteral 5) 2 17
                      , T.TokenData T.TSemicolon 2 18
                      , T.TokenData T.TCloseBrace 3 2
                      ]


main :: IO ()
main = do
    hspec $ test
