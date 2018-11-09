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
lexTokens = choice allTokens
    where
        allTokens = fmap try 
            [ genToken '{' TOpenBrace
            , genToken '}' TCloseBrace
            , genToken '(' TOpenParen
            , genToken ')' TCloseParen
            , genToken ';' TSemicolon
            , genToken '-' TNegation
            , genToken '~' TBitwiseComplement
            , genToken '!' TLogicalNegation
            , genReservedWordToken "int" TInt
            , genReservedWordToken "return" TReturn
            , tokenIntLiteral
            , tokenId
            ]

genToken :: Char -> Token -> Parser TokenData
genToken c t = do
    spaces
    _ <- char c
    (line, col) <- getTokenLocation
    return $ TokenData t line col

genReservedWordToken :: String -> Token -> Parser TokenData
genReservedWordToken w t = do
    spaces
    _ <- string w
    (line, col) <- getTokenLocation
    return $ TokenData t line col

getTokenLocation :: Parser (Int, Int)
getTokenLocation = do
    pos <- getPosition
    return (sourceLine pos, sourceColumn pos)

tokenIntLiteral :: Parser TokenData
tokenIntLiteral = do
    spaces 
    n <- choice 
        [ try readHexInt 
        , try readDecInt 
        , try readOctInt 
        , try readBinInt
        ]
    (line, col) <- getTokenLocation
    return $ TokenData (TIntLiteral n) line col

readDecInt :: Parser Int
readDecInt = do
    n <- many1 digit
    return $ read n

readOctInt :: Parser Int
readOctInt = do
    octalPrefix <- string "0o"
    n <- many1 octDigit
    return $ read $ octalPrefix ++ n

readHexInt :: Parser Int
readHexInt = do
    prefix <- string "0x"
    n <- many1 hexDigit
    return $ read $ prefix ++ n

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
    start <- spaces *> (try letter) <|> (char '_')
    rest <- many1 $ letter <|> char '_' <|> digit <|> char '-'
    (line, col) <- getTokenLocation
    return $ TokenData (TId $ [start] ++ rest) line col
