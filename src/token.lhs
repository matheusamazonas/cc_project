> module Token (Token(..), Operation(..), BasicType(..), PosToken) where

> import Text.Parsec.Pos

> type PosToken = (Token, SourcePos)

> data Operation = 
>     Minus 
>   | Plus 
>   | Times 
>   | Division 
>   | LessThan 
>   | LessOrEqual 
>   | GreaterThan 
>   | GreaterOrEqual
>   | Equals
>   | Different
>   | LogicalOr
>   | LogicalAnd
>   | LogicalNot
>   | ListConst
>   | Mod
>   deriving (Show, Eq)

> data BasicType = 
>      IntType
>    | BoolType
>    | CharType
>   deriving (Show, Eq)

> data Token =
>      TokenId String
>    | TokenNum Int
>    | TokenBool Bool
>    | TokenOp Operation
>    | TokenFuncDecl
>    | TokenFuncType
>    | TokenType BasicType
>    | TokenVoidType
>    | TokenOpenP
>    | TokenCloseP
>    | TokenOpenCurlyB
>    | TokenCloseCurlyB
>    | TokenOpenSquareB
>    | TokenCloseSquareB
>    | TokenIf
>    | TokenElse
>    | TokenWhile
>    | TokenVar
>    | TokenReturn
>    | TokenAttribution
>    | TokenPeriod
>    | TokenComma
>    | TokenEOL
>    | TokenChar Char
>    | TokenForAll
>    | TokenFunction
>   deriving (Show, Eq)

