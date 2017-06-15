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
>   deriving (Eq)

> data BasicType = 
>      IntType
>    | BoolType
>    | CharType
>   deriving (Eq)

> data Token =
>      TokenId String
>    | TokenNum Int
>    | TokenBool Bool
>    | TokenOp Operation
>    | TokenChar Char
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
>    | TokenForAll
>    | TokenFunction
>   deriving (Eq)

> instance Show Operation where
>   show Minus          = "-"
>   show Plus           = "+"
>   show Times          = "*"
>   show Division       = "/"
>   show LessThan       = "<"
>   show LessOrEqual    = "<="
>   show GreaterThan    = ">"
>   show GreaterOrEqual = ">="
>   show Equals         = "=="
>   show Different      = "!="
>   show LogicalOr      = "||"
>   show LogicalAnd     = "&&"
>   show LogicalNot     = "!"
>   show ListConst      = ":"
>   show Mod            = "%"

> instance Show BasicType where
>   show IntType  = "Int"
>   show BoolType = "Bool"
>   show CharType = "Char"

> instance Show Token where -- this instance is only used for error reporting, to make
>   show (TokenId _)       = "<identifier>" -- error messages more readable.
>   show (TokenNum _)      = "<int>"        -- above instances however can be used for
>   show (TokenBool _)     = "<bool>"       -- pretty-printing too
>   show (TokenChar _)     = "<char>"
>   show (TokenOp _)       = "<operation>"
>   show TokenFuncDecl     = "::"
>   show TokenFuncType     = "->"
>   show (TokenType _)     = "<type>"
>   show TokenVoidType     = "void"
>   show TokenOpenP        = "("
>   show TokenCloseP       = ")"
>   show TokenOpenCurlyB   = "{"
>   show TokenCloseCurlyB  = "}"
>   show TokenOpenSquareB  = "["
>   show TokenCloseSquareB = "]"
>   show TokenIf           = "if"
>   show TokenElse         = "else"
>   show TokenWhile        = "while"
>   show TokenVar          = "var"
>   show TokenReturn       = "return"
>   show TokenAttribution  = "="
>   show TokenPeriod       = "."
>   show TokenComma        = ","
>   show TokenEOL          = ";"
>   show TokenForAll       = "forall"
>   show TokenFunction     = "function"
