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
> printVarDecl i (GramVarDeclType t vId expr) = printType i False t ++ " " ++ printVarDeclTail 0 vId expr
> printVarDecl i (GramVarDeclVar vId expr) = tb i ++ "var " ++ printVarDeclTail 0 vId expr

> printFunDecl :: Int -> GramFuncDecl -> String
> printFunDecl i (GramFuncDecl (Id _ s) args [] stmts) = tb i ++ s ++ " " 
>           ++ tb i ++  "(" ++ printFArgs 0 args ++ ")\n{\n"
>           ++ printMany printStmt 1 stmts ++ "}\n"
> printFunDecl i (GramFuncDecl (Id _ s) args types stmts) = tb i ++ s ++ " " 
>           ++ tb i ++  "(" ++ printFArgs 0 args ++ ")"
>           ++ " :: " ++ printMany printFunType 0 types ++ "\n{\n"
>           ++ printMany printStmt 1 stmts ++ "}\n"

> printVarDeclTail :: Int -> GramId -> GramExp -> String
> printVarDeclTail i (Id _ s) expr = tb i ++ s ++ " = "++ printExpr 0 expr ++ ";\n"

> printRetType :: Int -> GramRetType -> String
> printRetType i (GramRetType t) = printType i False t
> printRetType i (GramVoidType _) = tb i ++ "Void"

> printFunType :: Int -> GramFunTypeAnnot -> String
> printFunType i (GramFunTypeAnnot ts r) = 
> 	        tb i ++ concat (map (printType 0 True) ts) ++ "-> " 
>           ++ printRetType 0 r 

> printType :: Int -> Bool -> GramType -> String
> printType i _ (GramBasicType _ t) = printBasicType i t
> printType i _ (GramTupleType _ t1 t2) = tb i ++ "(" ++ printType 0 True t1 ++ ", " ++ printType 0 True t2 ++ ")"
> printType i _ (GramListType _ t) = tb i ++ "[" ++ printType 0 True t ++ "]"
> printType i _ (GramIdType (Id _ s)) = tb i ++ s
> printType i False (GramFunType _ (GramFunTypeAnnot targs tret)) = tb i ++ "(" ++ printMany (\_ t -> printType 0 False t ++ " ") 0 targs ++ "-> " ++ printRetType 0 tret ++ ")"
> printType i True (GramFunType _ (GramFunTypeAnnot targs tret)) = tb i ++ printMany (\_ t -> printType 0 False t ++ " ") 0 targs ++ "-> " ++ printRetType 0 tret
> printType i False (GramForAllType _ boundids t) = tb i ++ "(forall " ++ printMany (\_ (Id _ id) -> id ++ " ") 0 boundids ++ ". " ++ printType 0 True t ++ ")"
> printType i True (GramForAllType _ boundids t) = tb i ++ "forall " ++ printMany (\_ (Id _ id) -> id ++ " ") 0 boundids ++ ". " ++ printType 0 True t

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
>         printTypes [t] = printType 0 False t
>         printTypes (t:ts) = printType 0 False t ++ "," ++ printTypes ts
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
> printExpr i (GramOverloadedBinary _ t op expr1 expr2) = tb i ++ "(" ++ printExpr 0 expr1 ++ " /*" ++ printType 0 False t ++ "*/" ++ printOp op ++ " " ++ printExpr 0 expr2 ++ ")"
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
























