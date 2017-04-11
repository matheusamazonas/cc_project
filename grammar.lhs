> module Grammar where

> import Token (BasicType, Operation)
> import Text.Parsec.Pos (SourcePos)

> data GramId = Id SourcePos String
>   deriving (Show, Eq)

> type Gram = [GramDecl]

> data GramVar = 
>      Var GramId [GramField]
>   deriving (Show, Eq)

> data GramFunType = 
>      GramFunType [GramFTypes] GramRetType
>   deriving (Show, Eq)

> data GramFTypes =
>      GramFTypes GramType [GramFTypes]
>   deriving (Show, Eq)

> data GramRetType =
>      GramRetType GramType
>    | GramVoidType SourcePos
>   deriving (Show, Eq)

> data GramType =
>      GramBasicType SourcePos BasicType
>    | GramTupleType SourcePos GramType GramType 
>    | GramListType SourcePos GramType 
>    | GramIdType GramId
>   deriving (Show, Eq)

> data GramFArgs = 
>      GramFArgsId GramId [GramFArgs]
>   deriving (Show, Eq)

> data GramExp = 
>      GramBinary SourcePos Operation GramExp GramExp
>    | GramUnary SourcePos Operation GramExp
>    | GramBool SourcePos Bool 
>    | GramChar SourcePos Char 
>    | GramNum SourcePos Int 
>    | GramExpFunCall GramFunCall
>    | GramExpId GramVar
>    | GramExpTuple SourcePos GramExp GramExp
>    | GramEmptyList SourcePos
>   deriving (Show, Eq)

> data GramArgList = 
>      GramArgList [GramActArgs]
>   deriving (Show, Eq)

> data GramFunCall = 
>     GramFunCall GramId GramArgList
>   deriving (Show, Eq)

> data GramActArgs = 
>      GramActExpr GramExp [GramActArgs]
>   deriving (Show, Eq)

> data GramField = 
>      First SourcePos [GramField] 
>    | Second SourcePos [GramField] 
>    | Head SourcePos [GramField] 
>    | Tail SourcePos [GramField] 
>   deriving (Show, Eq)

> data GramStmt = 
>      GramIf SourcePos GramExp [GramStmt] [GramStmt]
>    | GramWhile SourcePos GramExp [GramStmt] 
>    | GramAttr SourcePos GramVar GramExp 
>    | GramStmtFunCall GramFunCall
>    | GramReturn SourcePos (Maybe GramExp)
>    | GramFunVarDecl GramVarDecl
>   deriving (Show, Eq)   

> data GramVarDecl = 
>      GramVarDeclType GramType GramVarDeclTail
>    | GramVarDeclVar GramVarDeclTail
>   deriving (Show, Eq)

> data GramVarDeclTail = 
>      GramVarDeclTail GramId GramExp
>   deriving (Show, Eq)

> data GramFuncDeclTail = 
>      GramFuncDeclTail [GramFArgs] [GramFunType] [GramStmt]
>   deriving (Show, Eq)

> data GramFuncDecl = 
>      GramFuncDecl GramId GramFuncDeclTail
>   deriving (Show, Eq)

> data GramDecl = 
>      GramDeclFun GramFuncDecl
>    | GramDeclVar GramVarDecl
>   deriving (Show, Eq)
