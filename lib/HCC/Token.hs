module HCC.Token where

data TokenData = TokenData
    { token        :: Token
    , lineNumber   :: Int
    , columnNumber :: Int
    }
    deriving (Eq, Show)

data Token
    = TOpenBrace
    | TCloseBrace
    | TOpenParen
    | TCloseParen
    | TSemicolon
    | TInt
    | TReturn
    | TIntLiteral Int
    | TId String

instance Eq Token where
    (TOpenBrace)    == (TOpenBrace)    = True
    (TOpenBrace)    == (_)             = False
    (TCloseBrace)   == (TCloseBrace)   = True
    (TCloseBrace)   == (_)             = False
    (TOpenParen)    == (TOpenParen)    = True
    (TOpenParen)    == (_)             = False
    (TCloseParen)   == (TCloseParen)   = True
    (TCloseParen)   == (_)             = False
    (TSemicolon)    == (TSemicolon)    = True
    (TSemicolon)    == (_)             = False
    (TInt)          == (TInt)          = True
    (TInt)          == (_)             = False
    (TReturn)       == (TReturn)       = True
    (TReturn)       == (_)             = False
    (TIntLiteral _) == (TIntLiteral _) = True
    (TIntLiteral _) == (_)             = False
    (TId _)         == (TId _)         = True
    (TId _)         == (_)             = False
    

instance Show Token where
    show TOpenBrace      = "{"
    show TCloseBrace     = "}"
    show TOpenParen      = "("
    show TCloseParen     = ")"
    show TSemicolon      = ";"
    show TInt            = "INT"
    show TReturn         = "RET"
    show (TIntLiteral n) = show n
    show (TId x)         = "Identifier \"" ++ x ++ "\""
