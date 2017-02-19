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

> data Token =
>      TokenNum Int
>    | TokenOp Operation
>    | TokenOpenP
>    | TokenCloseP
>    | TokenEOL
>   deriving (Show)


> lexer :: String -> [Token]
> lexer [] = []
> lexer (x:xs)
>   | isSpace x = lexer xs 
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


> lexNum :: String -> [Token]
> lexNum s = TokenNum (read num) : lexer rest
>   where
>     (num, rest) = span isDigit s