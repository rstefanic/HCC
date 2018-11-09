module HCC.Gen where

import qualified HCC.AST as AST

generate :: AST.Program -> String
generate (AST.Program func) = emitFunction func

emitFunction :: AST.Function -> String
emitFunction (AST.Function name _ statement) = 
    ".globl " ++ name ++ "\n" ++
    name ++ ":\n" ++ (emitReturn statement)
    
emitReturn :: AST.Statement -> String
emitReturn (AST.ReturnValue expr) = emitExpr expr ++ "\tret\n"

emitExpr :: AST.Expression -> String
emitExpr (AST.Constant n) = "\tmovl\t$" ++ show n ++ ", %eax\n"
emitExpr (AST.UnOp AST.Negation expr) = emitExpr expr ++ "\tneg %eax\n"
emitExpr (AST.UnOp AST.BitwiseComplement expr) = emitExpr expr ++ "\tnot %eax\n"
emitExpr (AST.UnOp AST.LogicalNegation expr) = emitExpr expr ++ 
    "\tcmpl\t%0, %eax\n" ++
    "\tmovl\t%0, %eax\n" ++
    "\tsete\t%al\n"
