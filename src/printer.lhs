> {-# LANGUAGE UnicodeSyntax #-}
> module Printer (prettyPrint, printGram) where

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
> printVar i (Var (Id _ varId) fields) = 
>          	tb i ++ varId ++ printMany printField i fields

> printOp :: Operation -> String
> printOp Minus          = "-"
> printOp Plus           = "+"
> printOp Times          = "*"
> printOp Division       = "/"
> printOp LessThan       = "<"
> printOp LessOrEqual    = "<="
> printOp GreaterThan    = ">"
> printOp GreaterOrEqual = ">="
> printOp Equals         = "=="
> printOp Different      = "!="
> printOp LogicalOr      = "||"
> printOp LogicalAnd     = "&&"
> printOp LogicalNot     = "!"
> printOp ListConst      = ":"
> printOp Mod            = "%"

> printGram :: Gram -> String
> printGram decls = printMany printDecl 0 decls

> printDecl :: Int -> GramDecl -> String
> printDecl i (GramDeclFun fDecl) = printFunDecl i fDecl
> printDecl i (GramDeclVar vDecl) = printVarDecl i vDecl

> printVarDecl :: Int -> GramVarDecl -> String
> printVarDecl i (GramVarDeclType t declTail) = printType i t ++ " " ++ printVarDeclTail 0 declTail
> printVarDecl i (GramVarDeclVar declTail) = tb i ++ "var " ++ printVarDeclTail 0 declTail

> printFunDecl :: Int -> GramFuncDecl -> String
> printFunDecl i (GramFuncDecl (Id _ s) declTail) = tb i ++ s ++ " " ++ printFuncDeclTail 0 declTail

> printVarDeclTail :: Int -> GramVarDeclTail -> String
> printVarDeclTail i (GramVarDeclTail (Id _ s) expr) = tb i ++ s ++ " = "++ printExpr 0 expr ++ ";\n"

> printFuncDeclTail :: Int -> GramFuncDeclTail -> String
> printFuncDeclTail i (GramFuncDeclTail args [] stmts) = 
>           tb i ++  "(" ++ printFArgs 0 args ++ ")\n{\n"
>           ++ printMany printStmt 1 stmts ++ "}\n"
> printFuncDeclTail i (GramFuncDeclTail args funTypes stmts) = 
>           tb i ++  "(" ++ printFArgs 0 args ++ ")"
>           ++ " :: " ++ printMany printFunType 0 funTypes ++ "\n{\n"
>           ++ printMany printStmt 1 stmts ++ "}\n"

> printRetType :: Int -> GramRetType -> String
> printRetType i (GramRetType t) = printType i t
> printRetType i (GramVoidType _) = tb i ++ "Void"

> printFunType :: Int -> GramFunType -> String
> printFunType i (GramFunType ts r) = 
> 	        tb i ++ printMany printFTypes 0 ts ++ "-> " 
>           ++ printRetType 0  r 

> printFTypes :: Int -> GramFTypes -> String
> printFTypes i (GramFTypes t ts) = tb i ++ printType i t ++ " " ++ printMany printFTypes 0 ts

> printType :: Int -> GramType -> String
> printType i (GramBasicType _ t) = printBasicType i t
> printType i (GramTupleType _ t1 t2) = tb i ++ "(" ++ printType 0 t1 ++ ", " ++ printType 0 t2 ++ ")"
> printType i (GramListType _ t) = tb i ++ "[" ++ printType 0 t ++ "]"
> printType i (GramIdType (Id _ s)) = tb i ++ s

> printBasicType :: Int -> BasicType -> String
> printBasicType i IntType = tb i ++ "Int"
> printBasicType i BoolType = tb i ++ "Bool"
> printBasicType i CharType = tb i ++ "Char"

> printFArgs :: Int -> [GramId] -> String
> printFArgs i [] = ""
> printFArgs i ((Id _ a):[]) = tb i ++ a
> printFArgs i ((Id _ a):as) = tb i ++ a ++ ", " ++ printFArgs 0 as

> printStmt :: Int -> GramStmt -> String
> printStmt i (GramIf _ expr ifS elseS) = printIf i expr ifS elseS 
> printStmt i (GramWhile _ expr stmts) = printWhile i expr stmts
> printStmt i (GramReturn _ exprs) = printReturn i exprs
> printStmt i (GramFunVarDecl vardecl) = printVarDecl i vardecl
> printStmt i (GramAttr _ var expr) = printAttr i var expr
> printStmt i (GramStmtFunCall call) = printFuncall i call ++ ";\n"

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
> printFuncall i (GramOverloadedFunCall ts (Id _ fId) args) = tb i ++ "/*" ++ printTypes ts ++ "*/" ++ fId ++ "(" ++ printArgs 0 args ++ ")"
>   where printTypes [] = ""
>         printTypes [t] = printType 0 t
>         printTypes (t:ts) = printType 0 t ++ "," ++ printTypes ts
> printFuncall i (GramFunCall (Id _ fId) args) = 
>           tb i ++ fId ++ "(" 
>           ++ printArgs 0 args ++ ")"

> printReturn :: Int -> (Maybe GramExp) -> String
> printReturn i Nothing = tb i ++ "return;\n"
> printReturn i (Just expr) = 
>      	    tb i ++ "return " 
>           ++ printExpr 0 expr ++ ";\n"

> printExpr :: Int -> GramExp -> String
> printExpr i (GramBool _ b) = tb i ++ show b 
> printExpr i (GramChar _ c) = tb i ++ show c 
> printExpr i (GramNum _ n)  = tb i ++ show n 
> printExpr i (GramEmptyList _) = tb i ++ "[]"
> printExpr i (GramExpTuple _ expr1 expr2) = tb i ++ "(" ++ printExpr 0 expr1 ++ ", " ++ printExpr 0 expr2 ++ ")"
> printExpr i (GramOverloadedBinary _ t op expr1 expr2) = tb i ++ "(" ++ printExpr 0 expr1 ++ " /*" ++ printType 0 t ++ "*/" ++ printOp op ++ " " ++ printExpr 0 expr2 ++ ")"
> printExpr i (GramBinary _ op expr1 expr2) = tb i ++ "(" ++ printExpr 0 expr1 ++ " " ++ printOp op ++ " " ++ printExpr 0 expr2 ++ ")"
> printExpr i (GramUnary _ op expr) = tb i ++ printOp op ++ printExpr 0 expr
> printExpr i (GramExpFunCall call) = printFuncall i call
> printExpr i (GramExpId var) = printVar i var

> printArgs :: Int -> [GramExp] -> String
> printArgs i [] = tb i
> printArgs i [arg] = tb i ++ printExpr i arg
> printArgs i (arg:args) = tb i ++ printExpr i arg ++ ", " ++ printArgs i args

> printField :: Int -> GramField -> String
> printField i (First _ fields) = ".fst" ++ printMany printField i fields
> printField i (Second _ fields) = ".snd" ++ printMany printField i fields
> printField i (Head _ fields) = ".hd" ++ printMany printField i fields
> printField i (Tail _ fields) = ".tl" ++ printMany printField i fields
























