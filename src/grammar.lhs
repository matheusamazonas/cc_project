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
>    | GramIdType GramId
>    | GramTupleType SourcePos GramType GramType 
>    | GramListType SourcePos GramType 
>   deriving (Show, Eq)

> data GramExp = 
>      GramBool SourcePos Bool 
>    | GramChar SourcePos Char 
>    | GramNum SourcePos Int 
>    | GramEmptyList SourcePos
>    | GramExpTuple SourcePos GramExp GramExp
>    | GramBinary SourcePos Operation GramExp GramExp
>    | GramOverloadedBinary SourcePos GramType Operation GramExp GramExp
>    | GramUnary SourcePos Operation GramExp
>    | GramExpId GramVar
>    | GramExpFunCall GramFunCall
>   deriving (Show, Eq)

> data GramFunCall = 
>      GramFunCall GramId [GramExp]
>    | GramOverloadedFunCall [GramType] GramId [GramExp]
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
>    | GramReturn SourcePos (Maybe GramExp)
>    | GramFunVarDecl GramVarDecl
>    | GramAttr SourcePos GramVar GramExp 
>    | GramStmtFunCall GramFunCall
>   deriving (Show, Eq)   

> data GramVarDecl = 
>      GramVarDeclType GramType GramVarDeclTail
>    | GramVarDeclVar GramVarDeclTail
>   deriving (Show, Eq)

> data GramVarDeclTail = 
>      GramVarDeclTail GramId GramExp
>   deriving (Show, Eq)

> data GramFuncDeclTail = 
>      GramFuncDeclTail [GramId] [GramFunType] [GramStmt]
>   deriving (Show, Eq)

> data GramFuncDecl = 
>      GramFuncDecl GramId GramFuncDeclTail
>   deriving (Show, Eq)

> data GramDecl = 
>      GramDeclFun GramFuncDecl
>    | GramDeclVar GramVarDecl
>   deriving (Show, Eq)
