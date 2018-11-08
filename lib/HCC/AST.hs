{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}

module HCC.AST where

type Name = String
type Type = String
type Indent = Int

data Program = Program Function
    deriving (Eq, Show)

data Function = Function Name Type Statement
    deriving (Eq, Show)

data Statement = ReturnValue Expression
    deriving (Eq, Show)

data Expression = Constant Int
    deriving Eq

instance Show Expression where
    show (Constant x) = "<" ++ (show x) ++ ">"

class Show a => PrettyPrint a where
    pprint :: Indent -> a -> String

instance PrettyPrint Statement where
    pprint n (ReturnValue expr) = (replicate n '\t' ++ ("RET ") ++ (show expr))

instance PrettyPrint Function where
    pprint n (Function name t statement) = 
        (replicate n '\t' ++ ("FUNC " ++ show t ++ " " ++ name ++ ": \n")) ++ 
            (pprint (n + 1) statement)

instance PrettyPrint Program where
    pprint n (Program func) = (replicate n '\t' ++ ("PROG: \n")) ++ 
        (pprint (n + 1) func)
