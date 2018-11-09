{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module HCC.Parser where

import Control.Monad.Except
import Control.Monad.State

import qualified HCC.AST as AST
import HCC.Token

data ParseError 
    = UnexpectedToken TokenData
    | UnexpectedEof String
    deriving Eq

instance Show ParseError where
    show (UnexpectedToken (TokenData t l c)) = 
        "Unexpected token " ++ show t ++ " on line " ++ show l ++ " column " ++ show c
    show (UnexpectedEof x) = "Unexpected EOF where token should be: " ++ x

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
        [] -> throwError $ UnexpectedEof "sat"

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
    t <- get
    case t of
        (TokenData (TIntLiteral n) _ _):ts -> do
            put ts
            return $ AST.Constant n
        tdata:ts -> do
            if testForUnOp tdata
                then do
                    u <- parseUnaryOp
                    put ts
                    e <- parseExpression
                    return $ AST.UnOp u e
                else throwError (UnexpectedToken tdata)
        [] -> throwError $ UnexpectedEof "Parsing Expression"
            

testForUnOp :: TokenData -> Bool
testForUnOp (TokenData unop _ _) = case unop of
    TNegation          -> True
    TBitwiseComplement -> True
    TLogicalNegation   -> True
    _                  -> False

parseUnaryOp :: TokenParser AST.UnaryOp
parseUnaryOp = do
    t <- get
    case t of
        (TokenData TNegation _ _):ts -> do
            put ts
            return $ AST.Negation
        (TokenData TBitwiseComplement _ _):ts -> do
            put ts
            return $ AST.BitwiseComplement
        (TokenData TLogicalNegation _ _):ts -> do
            put ts
            return $ AST.LogicalNegation
        tdata:_ -> throwError (UnexpectedToken tdata)
        [] -> throwError $ UnexpectedEof "Parsing UnOp"
        
parseNegation :: TokenParser AST.UnaryOp
parseNegation = do
    consume TNegation
    return $ AST.Negation

consume :: Token -> TokenParser ()
consume tok = satisfy (== tok) >> return ()

parseToken :: Token -> TokenParser String
parseToken t = satisfy (== t) >> (return $ show t)

getId :: TokenParser AST.Name
getId = satisfy (== (TId "name")) >>= \(TId name) -> return name

getTIntLiteral :: TokenParser Int
getTIntLiteral = satisfy (== (TIntLiteral 0)) >>= \(TIntLiteral n) -> return n
