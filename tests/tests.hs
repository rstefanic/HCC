module Main where

import Test.Hspec

import HCC.Lexer
import qualified HCC.Token as T

test :: Spec
test = do
    describe "Should return true" $ do
        it "True is True" $ do
            True `shouldBe` True

basicParsing :: String -> Spec
basicParsing file = do
    describe "A basic main function should compile" $ do
        it "The Lexer should be properly tokenize the input" $ do
            HCC.Lexer.lex file `shouldBe` 
                Right [ T.TokenData T.TInt 1
                      , T.TokenData (T.TId "main") 1
                      , T.TokenData T.TOpenParen 1
                      , T.TokenData T.TCloseParen 1
                      , T.TokenData T.TOpenBrace 1
                      , T.TokenData T.TReturn 2
                      , T.TokenData (T.TIntLiteral 5) 2
                      , T.TokenData T.TSemicolon 2
                      , T.TokenData T.TCloseBrace 3
                      ]

main :: IO ()
main = do
    basicMain <- readFile "./tests/basic_main.c"
    hspec $ basicParsing basicMain
