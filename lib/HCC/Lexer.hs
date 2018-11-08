module HCC.Lexer where

import Control.Applicative ((*>), (<|>))
import Data.Char           (digitToInt)
import Text.Parsec
    ( parse
    , try
    , many1
    , char
    , spaces
    , digit
    , letter
    , string
    , getPosition
    , sourceLine
    , sourceColumn
    , hexDigit
    , octDigit
    , oneOf
    , choice
    )
import Text.Parsec.Error  (ParseError)
import Text.Parsec.String (Parser)

import HCC.Token

lex :: String -> Either ParseError [TokenData]
lex = parse (many1 lexTokens) "Tokenize"

lexTokens :: Parser TokenData
lexTokens = 
        try tokenOpenBrace
    <|> try tokenCloseBrace
    <|> try tokenOpenParen
    <|> try tokenCloseParen
    <|> try tokenSemicolon
    <|> try tokenInt
    <|> try tokenReturn
    <|> try tokenIntLiteral
    <|> try tokenId

getTokenLocation :: Parser (Int, Int)
getTokenLocation = do
    pos <- getPosition
    return (sourceLine pos, sourceColumn pos)

tokenOpenBrace :: Parser TokenData
tokenOpenBrace = do
    _ <- spaces *> char '{'
    (line, col) <- getTokenLocation
    return $ TokenData TOpenBrace line col

tokenCloseBrace :: Parser TokenData
tokenCloseBrace = do
    _ <- spaces *> char '}'
    (line, col) <- getTokenLocation
    return $ TokenData TCloseBrace line col

tokenOpenParen :: Parser TokenData
tokenOpenParen = do
    _ <- spaces *> char '('
    (line, col) <- getTokenLocation
    return $ TokenData TOpenParen line col

tokenCloseParen :: Parser TokenData
tokenCloseParen = do
    _ <- spaces *> char ')'
    (line, col) <- getTokenLocation
    return $ TokenData TCloseParen line col

tokenSemicolon :: Parser TokenData
tokenSemicolon = do
    _ <- spaces *> char ';'
    (line, col) <- getTokenLocation
    return $ TokenData TSemicolon line col

tokenNegation :: Parser TokenData
tokenNegation = do
    _ <- spaces *> char '-'
    (line, col) <- getTokenLocation
    return $ TokenData TNegation line col

tokenBitwiseComplement :: Parser TokenData
tokenBitwiseComplement = do
    _ <- spaces *> char '~'
    (line, col) <- getTokenLocation
    return $ TokenData TBitwiseComplement line col

tokenLogicalNegation :: Parser TokenData
tokenLogicalNegation = do
    _ <- spaces *> char '!'
    (line, col) <- getTokenLocation
    return $ TokenData TLogicalNegation line col

tokenInt :: Parser TokenData
tokenInt = do
    _ <- spaces *> string "int"
    (line, col) <- getTokenLocation
    return $ TokenData  TInt line col

tokenReturn :: Parser TokenData
tokenReturn = do
    _ <- spaces *> string "return"
    (line, col) <- getTokenLocation
    return $ TokenData TReturn line col

tokenIntLiteral :: Parser TokenData
tokenIntLiteral = do
    spaces 
    n <- choice [readOctInt, readDecInt, readHexInt, readBinInt]
    (line, col) <- getTokenLocation
    return $ TokenData (TIntLiteral n) line col

readDecInt :: Parser Int
readDecInt = read <$> many1 digit

readOctInt :: Parser Int
readOctInt = do
    octalPrefix <- string "0o"
    n <- many1 octDigit
    return $ read $ octalPrefix ++ n

readHexInt :: Parser Int
readHexInt = do
    _ <- string "0x"
    n <- many1 hexDigit
    return $ read n

readBinInt :: Parser Int
readBinInt = do
    _ <- string "0b"
    n <- many1 $ oneOf "01"
    return $ binToDec n

binToDec :: String -> Int
binToDec = foldr step 0
    where step x y = (+) (digitToInt x) ((*) y 2)

tokenId :: Parser TokenData
tokenId = do
    identifier <- spaces *> 
        ( many1
        $ letter
        <|> char '-'
        <|> char '_'
        <|> digit
        )
    (line, col) <- getTokenLocation
    return $ TokenData (TId identifier) line col
