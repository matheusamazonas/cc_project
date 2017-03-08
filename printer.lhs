> {-# LANGUAGE UnicodeSyntax #-}
> module Print (prettyPrint) where

> import Grammar
> import Lexer
> import Test
> import Token

> tb :: Int -> String
> tb n = replicate n '\t'

> printMany :: (Int -> a -> String) -> Int -> [a] -> String
> printMany f i xs = concat $ map (f i) xs

> prettyPrint :: String -> IO()
> prettyPrint s = putStrLn $ printGram tree
>   where 
>     (Right tree) = parseSPL $ lexer nP s

> printVar :: Int -> GramVar -> String
> printVar i (Var varId fields) = 
>          	tb i ++ varId ++ printMany printField i fields

> printOp :: Operation -> String
> printOp Minus = "-"
> printOp Plus = "+"
> printOp Times = "*"
> printOp Division = "/"
> printOp LessThan = "<"
> printOp LessOrEqual = "<="
> printOp GreaterThan = ">"
> printOp GreatherOrEqual= ">="
> printOp Equals = "=="
> printOp Different = "!="
> printOp LogicalOr = "||"
> printOp LogicalAnd = "&&"
> printOp LogicalNot = "!"
> printOp ListConst = ":"
> printOp Mod = "%"

> printGram :: Gram -> String
> printGram decls = printMany printDecl 0 decls

> printDecl :: Int -> GramDecl -> String
> printDecl i (GramDeclFun fDecl) = printFunDecl i fDecl
> printDecl i (GramDeclVar vDecl) = printVarDecl i vDecl

> printVarDecl :: Int -> GramVarDecl -> String
> printVarDecl i (GramVarDeclType t declTail) = printType i t ++ " " ++ printVarDeclTail 0 declTail
> printVarDecl i (GramVarDeclVar declTail) = tb i ++ "var " ++ printVarDeclTail 0 declTail

> printFunDecl :: Int -> GramFuncDecl -> String
> printFunDecl i (GramFuncDecl s declTail) = tb i ++ s ++ " " ++ printFuncDeclTail 0 declTail

> printVarDeclTail :: Int -> GramVarDeclTail -> String
> printVarDeclTail i (GramVarDeclTail s expr) = tb i ++ s ++ " = "++ printExpr 0 expr ++ ";\n"

> printFuncDeclTail :: Int -> GramFuncDeclTail -> String
> printFuncDeclTail i (GramFuncDeclTail args funTypes varDecls stmts) = 
>           tb i ++  "(" ++ printMany printFArgs 0 args ++ ")"
>           ++ " :: " ++ printMany printFunType 0 funTypes ++ "\n{\n"
>           ++ printMany printVarDecl 1 varDecls 
>           ++ printMany printStmt 1 stmts ++ "}"

> printRetType :: Int -> GramRetType -> String
> printRetType i (GramRetType t) = printType i t
> printRetType i GramVoidType = tb i ++ "void"

> printFunType :: Int -> GramFunType -> String
> printFunType i (GramFunType ts r) = 
> 	        tb i ++ printMany printFTypes 0 ts ++ "-> " 
>           ++ printRetType 0  r 

> printFTypes :: Int -> GramFTypes -> String
> printFTypes i (GramFTypes t ts) = tb i ++ printType i t ++ " " ++ printMany printFTypes 0 ts

> printType :: Int -> GramType -> String
> printType i (GramBasicType t) = printBasicType i t
> printType i (GramTupleType t1 t2) = tb i ++ "(" ++ printType 0 t1 ++ ", " ++ printType 0 t2 ++ ")"
> printType i (GramListType t) = tb i ++ "[" ++ printType 0 t ++ "]"
> printType i (GramIdType s) = tb i ++ s

> printBasicType :: Int -> BasicType -> String
> printBasicType i IntType = tb i ++ "Int"
> printBasicType i BoolType = tb i ++ "Bool"
> printBasicType i CharType = tb i ++ "Char"

> printFArgs :: Int -> GramFArgs -> String
> printFArgs i (GramFArgsId s []) = tb i ++ s 
> printFArgs i (GramFArgsId s fArgs) = tb i ++ s ++ ", " ++ printMany printFArgs 0 fArgs

> printStmt :: Int -> GramStmt -> String
> printStmt i (GramIf expr ifS elseS) = printIf i expr ifS elseS 
> printStmt i (GramWhile expr stmts) = printWhile i expr stmts
> printStmt i (GramAttr var expr) = printAttr i var expr
> printStmt i (GramStmtFunCall call) = printFuncall i call ++ ";\n"
> printStmt i (GramReturn exprs) = printReturn i exprs

> printIf :: Int -> GramExp -> [GramStmt] -> [GramStmt] -> String
> printIf i expr ifS elseS = 
>           tb i ++ "if (" 
>       	++ printExpr 0 expr ++ ")\n" 
>           ++ tb i ++ "{\n" 
>           ++ printMany printStmt (i+1) ifS 
>         	++ tb i ++ "}\n"
>           ++ printElse i elseS

> printElse :: Int -> [GramStmt] -> String
> printElse i [] = ""
> printElse i ss = 
>           tb i ++ "else\n"
>           ++ tb i ++ "{\n" 
>           ++ printMany printStmt (i+1) ss 
>           ++ tb i ++ "}\n"

> printWhile :: Int -> GramExp -> [GramStmt] -> String
> printWhile i expr ss = 
>           tb i ++ "while ("
>           ++ printExpr 0 expr ++ ")\n"
>           ++ tb i ++ "{\n"
>           ++ printMany printStmt (i+1) ss 
>           ++ tb i ++ "}\n"

> printAttr :: Int -> GramVar -> GramExp -> String
> printAttr i var expr = 
>           printVar i var
>           ++ " = " ++ printExpr 0 expr ++ ";\n"

> printFuncall :: Int -> GramFunCall -> String
> printFuncall i (GramFunCall fId args) = 
>           tb i ++ fId ++ "(" 
>           ++ printArgList 0 args ++ ")"

> printReturn :: Int -> [GramExp] -> String
> printReturn i exprs = 
>      	    tb i ++ "return " 
>           ++ printMany printExpr 0 exprs ++ ";\n"

> printExpr :: Int -> GramExp -> String
> printExpr i  (GramBinary op expr1 expr2) = "(" ++ printExpr i expr1 ++ printOp op ++ printExpr 0 expr2 ++ ")"
> printExpr i  (GramUnary op expr) = "(" ++ printOp op ++ printExpr 0 expr ++ ")" ++ ")"
> printExpr i  (GramBool b) = tb i ++ show b 
> printExpr i  (GramChar c) = tb i ++ show c 
> printExpr i  (GramNum n) = tb i ++ show n 
> printExpr i  (GramExpFunCall call) = printFuncall i call
> printExpr i  (GramExpId var) = printVar i var
> printExpr i  (GramExpTuple expr1 expr2) = tb i ++ "("++ printExpr 0 expr1 ++ ", " ++ printExpr 0 expr2 ++ ")"
> printExpr i  (GramEmptyList) = tb i ++ "[]"

> printArgList :: Int -> GramArgList -> String
> printArgList i (GramArgList args) = printMany printActArgs i args

> printActArgs :: Int -> GramActArgs -> String
> printActArgs i (GramActExpr expr []) = tb i ++ printExpr i expr 
> printActArgs i (GramActExpr expr args) = 
>           tb i ++ printExpr i expr 
>           ++ ", " ++ printMany printActArgs i args

> printField :: Int -> GramField -> String
> printField i (First fields) = ".fst" ++ printMany printField i fields
> printField i (Second fields) = ".snc" ++ printMany printField i fields
> printField i (Head fields) = ".hd" ++ printMany printField i fields
> printField i (Tail fields) = ".tl" ++ printMany printField i fields
























