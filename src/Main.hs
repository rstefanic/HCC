{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Semigroup      ((<>))
import           Data.Text           (pack)
import qualified Data.Text.IO as DTI (writeFile)
import           Options.Applicative

import HCC.Lexer
import HCC.Parser
import HCC.AST
import HCC.Gen

data Options = Options 
    { inputFile  :: String
    , outputFile :: Maybe String 
    }

main :: IO ()
main = do
    userOpts <- execParser opts
    sourceCode <- readFile $ inputFile userOpts
    tokenizedInput <- return $ HCC.Lexer.lex sourceCode
    case tokenizedInput of
        Left err -> putStrLn $ show err
        Right i -> case HCC.Parser.parse i of
                      Right ast' -> writeASMToFile ast' (outputFile userOpts)
                      Left err -> putStrLn $ show err
    where opts = info (options <**> helper)
                    ( fullDesc
                    <> progDesc "A simple C compiler written in Haskell."
                    <> header "HCC -- A basic C compiler"
                    )

options :: Parser Options
options = Options 
    <$> strOption
      ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "C code input."
      )
    <*> optional
      ( strOption
      $ long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Assembly file output."
      )

writeASMToFile :: Program -> Maybe String -> IO ()
writeASMToFile ast filename = do
    asm <- return $ generate ast
    case filename of
        Nothing -> DTI.writeFile "./a.s" (pack asm)
        Just file -> DTI.writeFile file (pack asm)
