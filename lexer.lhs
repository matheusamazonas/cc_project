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

>   deriving (Show)

> data Type = 
>      IntType
>    | BoolType
>    | CharType
>    | VoidType
>   deriving (Show)

> data Token =
>      TokenId String
>    | TokenNum Int
>    | TokenBool Bool
>    | TokenOp Operation
>    | TokenType Type
>    | TokenOpenP
>    | TokenCloseP
>    | TokenIf
>    | TokenWhile
>    | TokenVar
>    | TokenReturn
>    | TokenEOL
>   deriving (Show)


> lexer :: String -> [Token]
> lexer [] = []
> lexer (x:xs)
>   | isSpace x = lexer xs 
>   | isAlpha x = lexText (x:xs)
>   | isDigit x = lexNum (x:xs)
> lexer ('+':xs)     = TokenOp Plus             : lexer xs
> lexer ('-':xs)     = TokenOp Minus            : lexer xs
> lexer ('*':xs)     = TokenOp Times            : lexer xs
> lexer ('/':xs)     = TokenOp Division         : lexer xs
> lexer ('(':xs)     = TokenOpenP               : lexer xs
> lexer (')':xs)     = TokenCloseP              : lexer xs
> lexer ('<':'=':xs) = TokenOp LessOrEqual      : lexer xs
> lexer ('>':'=':xs) = TokenOp GreatherOrEqual  : lexer xs
> lexer ('<':xs)     = TokenOp LessThan         : lexer xs
> lexer ('>':xs)     = TokenOp GreaterThan      : lexer xs
> lexer (';':xs)     = TokenEOL                 : lexer xs


> lexText :: String -> [Token]
> lexText s = lexId id : lexer rest
>   where
>     (id, rest) = span (isAlphaNum ||| ((==) '_')) s

> lexId :: String -> Token
> lexId s
>   | s == "Int"     = TokenType IntType
>   | s == "Bool"    = TokenType BoolType
>   | s == "Char"    = TokenType CharType
>   | s == "Void"    = TokenType VoidType
>   | s == "True"    = TokenBool True
>   | s == "False"    = TokenBool False
>   | s == "if"      = TokenIf
>   | s == "while"   = TokenWhile
>   | s == "var"     = TokenVar
>   | s == "return"  = TokenReturn
>   | otherwise      = TokenId s


> lexNum :: String -> [Token]
> lexNum s = TokenNum (read num) : lexer rest
>   where
>     (num, rest) = span isDigit s


> (|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
> f ||| g = \x -> (f x) || (g x)