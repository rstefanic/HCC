{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module HCC.Parser where

import qualified HCC.AST as AST

import HCC.Token

import Control.Monad.Except
import Control.Monad.State

data ParseError 
    = UnexpectedToken TokenData
    | UnexpectedEof
    deriving Eq

instance Show ParseError where
    show (UnexpectedToken (TokenData t l c)) = 
        "Unexpected token " ++ show t ++ " on line " ++ show l ++ " column " ++ show c
    show (UnexpectedEof) = "Unexpected EOF where token should be."

newtype TokenParser a = TokenP 
    { runTokenP :: ExceptT ParseError (State [TokenData]) a }
    deriving (Functor, Applicative, Monad, 
        MonadError ParseError, MonadState [TokenData])

runParser :: TokenParser a -> [TokenData] -> Either ParseError (a, [TokenData])
runParser parser tokens =
    case (runState . runExceptT . runTokenP) parser tokens of
        (Left err, _) -> Left err
        (Right r, rest) -> Right (r, rest)

parseAll :: TokenParser AST.Program
parseAll = do
    func <- parseFunction
    return $ AST.Program func

parse :: [TokenData] -> Either ParseError AST.Program
parse = fmap fst . runParser parseAll

satisfy :: (Token -> Bool) -> TokenParser Token
satisfy predicate = do
    s <- get
    case s of
        (TokenData t _ _):ts | predicate t -> do
                put ts
                return t
        tdata:_ -> throwError (UnexpectedToken tdata)
        [] -> throwError UnexpectedEof

parseFunction :: TokenParser AST.Function
parseFunction = do
    t <- parseToken TInt
    funcName <- getId
    consume TOpenParen
    consume TCloseParen
    consume TOpenBrace
    statement <- parseStatement
    consume TCloseBrace
    return $ AST.Function funcName t statement

parseStatement :: TokenParser AST.Statement
parseStatement = do
    consume TReturn
    expr <- parseExpression
    consume TSemicolon
    return $ AST.ReturnValue expr
    
parseExpression :: TokenParser AST.Expression
parseExpression = do
    n <- getTIntLiteral
    return $ AST.Constant n

consume :: Token -> TokenParser ()
consume tok = satisfy (== tok) >> return ()

parseToken :: Token -> TokenParser String
parseToken t = satisfy (== t) >> (return $ show t)

getId :: TokenParser AST.Name
getId = satisfy (== (TId "name")) >>= \(TId name) -> return name

getTIntLiteral :: TokenParser Int
getTIntLiteral = satisfy (== (TIntLiteral 0)) >>= \(TIntLiteral n) -> return n
