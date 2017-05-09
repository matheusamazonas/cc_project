> module Generator where

> import Grammar
> import Token
> import Data.Char

> type Depth = Integer
> type Id = String
> type Code = String
> type Scope = [(Id, Depth)]
> type Environment = (Depth, [Scope])

> generate :: Environment -> Gram -> (Environment, String)
> generate e _ = env e $ "LDC 2\nLDC 3\nADD" ++ "\nTRAP 0"

> env :: Environment -> Code -> (Environment, Code)
> env e c = (e, c)

> addVar :: Environment -> Id -> Environment
> addVar (d, (s:ss)) id = (d+1, ((id, d):s):ss)

> lookupVar :: Environment -> Id -> Depth
> lookupVar (_, (s:_)) varId = case lookup varId s of
>                               Nothing -> -999
>                               Just d -> d

> generateExpr :: (Environment, Code) -> GramExp -> Code
> generateExpr (e, c) (GramBool _ True) = c ++ "ldc -1\n"
> generateExpr (e, c) (GramBool _ False) = c ++ "ldc 0\n"
> generateExpr (e, c) (GramChar _ char) = c ++ "ldc " ++ show (ord char) ++ "\n"
> generateExpr (e, c) (GramNum _ i) = c ++ "ldc " ++ show i ++ "\n"
> generateExpr (e, c) (GramBinary _ op expr1 expr2) = c ++ generateExpr (e, c) expr1 ++ generateExpr (e, c) expr2 ++ generateOperation op
> generateExpr (e, c) (GramUnary _ op expr) = c ++ generateExpr (e, c) expr ++ generateOperation op
> generateExpr (e, c) (GramExpId (Var (Id _ varId) fields)) = c ++ "ldl " ++ show (lookupVar e varId) ++ "\n"

> generateOperation :: Operation -> String
> generateOperation Minus          = "sub\n"
> generateOperation Plus           = "add\n"
> generateOperation Times          = "mul\n"
> generateOperation Division       = "div\n"
> generateOperation LessThan       = "lt\n"
> generateOperation LessOrEqual    = "le\n"
> generateOperation GreaterThan    = "gt\n"
> generateOperation GreaterOrEqual = "ge\n"
> generateOperation Equals         = "eq\n"
> generateOperation Different      = "ne\n"
> generateOperation LogicalOr      = "or\n"
> generateOperation LogicalAnd     = "and\n"
> generateOperation LogicalNot     = "not\n"
> generateOperation ListConst      = "???\n"
> generateOperation Mod            = "mod\n"

> generateStmt :: (Environment, Code) -> GramStmt -> (Environment, Code)
> generateStmt (e, c) (GramWhile _ expr stmts) = env e $
>            "while_start:" ++ (generateExpr (e, c) expr) ++ "brf while_end\n" ++
>            (concat $ map snd $ map (generateStmt (e, c)) stmts) ++ "bra while_start\n" ++ "while_end:"
> generateStmt (e, c) (GramIf _ expr thenStmts []) = env e $
>            (generateExpr (e, c) expr) ++ "brf fi\n" ++ 
>            (concat $ map snd $ map (generateStmt (e, c)) thenStmts) ++ "fi:"
> generateStmt (e, c) (GramIf _ expr thenStmts elseStmts) = env e $
>            (generateExpr (e, c) expr) ++ "brf else\n" ++ 
>            (concat $ map snd $ map (generateStmt (e, c)) thenStmts) ++ "bra fi\n" ++ 
>            "else: " ++ (concat $ map snd $ map (generateStmt (e, c)) elseStmts) ++ "fi:"
> generateStmt (e, c) (GramReturn _ (Nothing)) = env e $ c ++ "unlink\nret\n"
> generateStmt (e, c) (GramReturn _ (Just expr)) = env e $ c ++ (generateExpr (e, c) expr) ++ "unlink\nret\n"
> generateStmt (e, c) (GramFunVarDecl (GramVarDeclType t (GramVarDeclTail (Id _ varId) expr))) = env (addVar e varId) (generateExpr (e, c) expr)
> generateStmt (e, c) (GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr))) = env (addVar e varId) (generateExpr (e, c) expr)

> testStmt s = generateStmt ((0, [[]]), "") s

undefined


Right (GramIf undefined (GramBinary undefined LessThan (GramNum undefined 3) (GramNum undefined 7)) [GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id undefined "x") (GramNum undefined 4))),GramReturn undefined (Just (GramExpId (Var (Id undefined "x") [])))] [GramReturn undefined (Just (GramNum undefined 0))])

(GramIf "test_file" (line 1, column 1) (GramBinary "test_file" (line 1, column 7) LessThan (GramNum "test_file" (line 1, column 5) 3) (GramNum "test_file" (line 1, column 9) 7)) [GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id "test_file" (line 1, column 18) "x") (GramBinary "test_file" (line 1, column 23) Plus (GramNum "test_file" (line 1, column 22) 4) (GramNum "test_file" (line 1, column 24) 6)))),GramReturn "test_file" (line 1, column 27) (Just (GramExpId (Var (Id "test_file" (line 1, column 34) "x") [])))] [GramReturn "test_file" (line 1, column 46) (Just (GramNum "test_file" (line 1, column 53) 0))])


(GramIf undefined (GramBinary undefined LessThan (GramNum undefined 3) (GramNum undefined 7)) [GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id undefined "x") (GramBinary undefined Plus (GramNum undefined 4) (GramNum undefined 6)))),GramReturn undefined (Just (GramExpId (Var (Id undefined "x") [])))] [GramReturn undefined (Just (GramNum undefined 0))])




















