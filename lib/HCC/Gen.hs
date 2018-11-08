module HCC.Gen where

import qualified HCC.AST as AST

generate :: AST.Program -> String
generate (AST.Program func) = emitFunction func

emitFunction :: AST.Function -> String
emitFunction (AST.Function name _ statement) = 
    ".globl " ++ name ++ "\n" ++
    name ++ ":\n" ++ (emitReturn statement)
    
emitReturn :: AST.Statement -> String
emitReturn (AST.ReturnValue expr) = 
    "\tmovl\t$" ++ (emitExpr expr) ++ ", %eax\n" ++
    "\tret\n"

emitExpr :: AST.Expression -> String
emitExpr (AST.Constant n) = show n
