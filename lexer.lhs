> module Lexer ( Token(..), Operation(..), BasicType(..), lexer ) where

> import Data.Char

> data Operation = 
>     Minus 
>   | Plus 
>   | Times 
>   | Division 
>   | LessThan 
>   | LessOrEqual 
>   | GreaterThan 
>   | GreatherOrEqual
>   | Equals
>   | Different
>   | LogicalOr
>   | LogicalAnd
>   | LogicalNot
>   | ListConst
>   | Mod
>   deriving (Show)

> data BasicType = 
>      IntType
>    | BoolType
>    | CharType
>   deriving (Show)

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
>   deriving (Show)


> lexer :: String -> [Token]
> lexer [] = []
> lexer ('/':'/':xs) = lexSkipLine xs
> lexer ('/':'*':xs) = lexSkipBlock xs
> lexer (x:xs)
>   | isSpace x = lexer xs 
>   | isAlpha x = lexText (x:xs)
>   | isDigit x = lexNum (x:xs)
> lexer ('<':'=':xs) = TokenOp LessOrEqual      : lexer xs
> lexer ('>':'=':xs) = TokenOp GreatherOrEqual  : lexer xs
> lexer ('=':'=':xs) = TokenOp Equals           : lexer xs
> lexer ('!':'=':xs) = TokenOp Different        : lexer xs
> lexer ('&':'&':xs) = TokenOp LogicalAnd       : lexer xs
> lexer ('|':'|':xs) = TokenOp LogicalOr        : lexer xs
> lexer (':':':':xs) = TokenFuncDecl            : lexer xs
> lexer ('-':'>':xs) = TokenFuncType            : lexer xs
> lexer ('=':xs)     = TokenAttribution         : lexer xs
> lexer ('+':xs)     = TokenOp Plus             : lexer xs
> lexer ('-':xs)     = TokenOp Minus            : lexer xs
> lexer ('*':xs)     = TokenOp Times            : lexer xs
> lexer ('/':xs)     = TokenOp Division         : lexer xs
> lexer ('%':xs)     = TokenOp Mod         		: lexer xs
> lexer ('(':xs)     = TokenOpenP               : lexer xs
> lexer (')':xs)     = TokenCloseP              : lexer xs
> lexer ('<':xs)     = TokenOp LessThan         : lexer xs
> lexer ('>':xs)     = TokenOp GreaterThan      : lexer xs
> lexer ('!':xs)     = TokenOp LogicalNot       : lexer xs
> lexer (':':xs)     = TokenOp ListConst        : lexer xs
> lexer (';':xs)     = TokenEOL                 : lexer xs
> lexer ('{':xs)     = TokenOpenCurlyB          : lexer xs
> lexer ('}':xs)     = TokenCloseCurlyB         : lexer xs
> lexer ('[':xs)     = TokenOpenSquareB         : lexer xs
> lexer (']':xs)     = TokenCloseSquareB        : lexer xs
> lexer ('.':xs)     = TokenPeriod              : lexer xs
> lexer (',':xs)     = TokenComma               : lexer xs

> lexText :: String -> [Token]
> lexText s = lexId id : lexer rest
>   where
>     (id, rest) = span (isAlphaNum ||| ((==) '_')) s

> lexId :: String -> Token
> lexId s
>   | s == "Int"     = TokenType IntType
>   | s == "Bool"    = TokenType BoolType
>   | s == "Char"    = TokenType CharType
>   | s == "Void"    = TokenVoidType
>   | s == "True"    = TokenBool True
>   | s == "False"   = TokenBool False
>   | s == "if"      = TokenIf
>   | s == "else"    = TokenElse
>   | s == "while"   = TokenWhile
>   | s == "var"     = TokenVar
>   | s == "return"  = TokenReturn
>   | otherwise      = TokenId s

> lexNum :: String -> [Token]
> lexNum s = TokenNum (read num) : lexer rest
>   where
>     (num, rest) = span isDigit s

> lexSkipLine :: String -> [Token]
> lexSkipLine []        = []
> lexSkipLine ('\n':xs) = lexer xs
> lexSkipLine (x:xs)    = lexSkipLine xs

> lexSkipBlock :: String -> [Token]
> lexSkipBlock []           = []
> lexSkipBlock ('*':'/':ys) = lexer ys
> lexSkipBlock (x:xs)       = lexSkipBlock xs

> (|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
> f ||| g = \x -> (f x) || (g x)









