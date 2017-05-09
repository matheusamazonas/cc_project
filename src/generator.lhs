> module Generator where

> import Control.Monad.State
> import Control.Monad.Writer
> import Grammar
> import Token
> import Data.Char

> type Depth = Integer
> type Id = String
> type Code = String
> type Scope = [(Id, Depth)]
> type EnvType = (Depth, [Scope])

> type Environment = WriterT Code (State EnvType)



Code generation

Call with, e.g., run generateStmtBlock stmts, with stmts :: [GramStmt]
Once implemented, generate = run generateGram

> generate :: Gram -> Code
> generate g = "LDC 2\nLDC 3\nADD" ++ "\nTRAP 0"

> generateStmtBlock :: [GramStmt] -> Environment ()
> generateStmtBlock stmts = do
>   pushScope
>   generateStmtBlock' stmts
>   where generateStmtBlock' [] = popScope
>         generateStmtBlock' (stmt:stmts) = do
>           generateStmt stmt
>           generateStmtBlock' stmts

> generateStmt :: GramStmt -> Environment ()
> generateStmt (GramWhile _ expr stmts) = do
>   label "while_start"
>   generateExpr expr
>   write "brf while_end"
>   generateStmtBlock stmts
>   write "bra while_start"
>   label "while_end"
> generateStmt (GramIf _ expr thenStmts elseStmts) = do
>   generateExpr expr
>   write "brf else"
>   generateStmtBlock thenStmts 
>   write "bra fi"
>   label "else" 
>   generateStmtBlock elseStmts
>   label "fi"
> generateStmt (GramReturn _ (Nothing)) = write "unlink\nret"
> generateStmt (GramReturn _ (Just expr)) = do
>   generateExpr expr
>   write "unlink\nret"
> generateStmt (GramFunVarDecl (GramVarDeclType t (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr
> generateStmt (GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr


> generateExpr :: GramExp -> Environment ()
> generateExpr (GramBool _ True) = write "ldc -1"
> generateExpr (GramBool _ False) = write "ldc 0"
> generateExpr (GramChar _ char) = write $ "ldc " ++ show (ord char)
> generateExpr (GramNum _ i) = write $ "ldc " ++ show i
> generateExpr (GramBinary _ op expr1 expr2) = do
>   generateExpr expr1
>   generateExpr expr2
>   write $ generateOperation op
> generateExpr (GramUnary _ op expr) = do
>   generateExpr expr
>   write $ generateOperation op
> generateExpr (GramExpId (Var (Id _ varId) fields)) = do
>   varLoc <- lookupVar varId
>   write $ "ldl " ++ show varLoc

> generateOperation :: Operation -> String
> generateOperation Minus          = "sub"
> generateOperation Plus           = "add"
> generateOperation Times          = "mul"
> generateOperation Division       = "div"
> generateOperation LessThan       = "lt"
> generateOperation LessOrEqual    = "le"
> generateOperation GreaterThan    = "gt"
> generateOperation GreaterOrEqual = "ge"
> generateOperation Equals         = "eq" -- 
> generateOperation Different      = "ne" --
> generateOperation LogicalOr      = "or"
> generateOperation LogicalAnd     = "and"
> generateOperation LogicalNot     = "not"
> generateOperation ListConst      = "halt" --
> generateOperation Mod            = "mod"



Scope handlers

> pushScope :: Environment ()
> pushScope = do
>   (d, ss) <- get
>   put (d, []:ss)

> popScope :: Environment ()
> popScope = do
>   (d, s:ss) <- get
>   put (d, ss)



Variable handlers

> addVar :: Id -> Environment ()
> addVar id = do
>   (d, (s:ss)) <- get
>   put (d+1, ((id, d):s):ss)

> lookupVar :: Id -> Environment Depth
> lookupVar varId = do
>   (_, ss) <- get
>   return $ lookupVar' varId ss
>   where lookupVar' _ [] = -999
>         lookupVar' varId (s:ss) =
>           case lookup varId s of
>             Nothing -> lookupVar' varId ss
>             Just d -> d


Environment handlers

> run :: (t -> Environment ()) -> t -> Code
> run gen gram = evalState (execWriterT (gen gram)) initEnv

> label :: String -> Environment ()
> label l = tell (l ++ ": ")

> write :: String -> Environment ()
> write c = tell (c ++ "\n")

> initEnv :: EnvType
> initEnv = (1, [[]])
