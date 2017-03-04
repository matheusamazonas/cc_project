> module Grammar where

> import Token (BasicType, Operation)

> type GramId = String

> type Gram = [GramDecl]

> data GramVar = Var GramId [GramField]
>   deriving (Show, Eq)

> data GramFunType = 
>      GramFunType [GramFTypes] GramRetType
>   deriving (Show, Eq)

> data GramFTypes =
>      GramFTypes GramType [GramFTypes]
>   deriving (Show, Eq)

> data GramRetType =
>      GramRetType GramType
>    | GramVoidType
>   deriving (Show, Eq)

> data GramType =
>      GramBasicType BasicType
>    | GramTupleType GramType GramType
>    | GramListType GramType
>    | GramIdType GramId
>   deriving (Show, Eq)

> data GramFArgs = 
>      GramFArgsId GramId [GramFArgs]
>   deriving (Show, Eq)

> data GramExp = 
>      GramBinary Operation GramExp GramExp
>    | GramUnary Operation GramExp 
>    | GramBool Bool
>    | GramChar Char
>    | GramNum Int
>    | GramExpFunCall GramFunCall
>    | GramExpId GramVar
>    | GramExpTuple GramExp GramExp
>    | GramEmptyList
>   deriving (Show, Eq)

> data GramArgList = GramArgList [GramActArgs]
>   deriving (Show, Eq)

> data GramFunCall = 
>     GramFunCall GramId GramArgList
>   deriving (Show, Eq)

> data GramActArgs = GramActExpr GramExp [GramActArgs]
>   deriving (Show, Eq)

> data GramField = 
>      First [GramField] 
>    | Second [GramField] 
>    | Head [GramField]
>    | Tail [GramField]
>   deriving (Show, Eq)

> data GramStmt = 
>      GramIf GramExp [GramStmt] [GramStmt]
>    | GramWhile GramExp [GramStmt]
>    | GramAttr GramVar GramExp
>    | GramStmtFunCall GramFunCall
>    | GramReturn [GramExp] 
>   deriving (Show, Eq)   

> data GramVarDecl = 
>      GramVarDeclType GramType GramVarDeclTail
>    | GramVarDeclVar GramVarDeclTail
>   deriving (Show, Eq)

> data GramVarDeclTail = 
>      GramVarDeclTail GramId GramExp
>   deriving (Show, Eq)

> data GramFuncDeclTail = 
>      GramFuncDeclTail [GramFArgs] [GramFunType] [GramVarDecl] [GramStmt]
>   deriving (Show, Eq)

> data GramFuncDecl = 
>      GramFuncDecl GramId GramFuncDeclTail
>   deriving (Show, Eq)

> data GramDecl = 
>      GramDeclFun GramFuncDecl
>    | GramDeclVar GramVarDecl
>   deriving (Show, Eq)
