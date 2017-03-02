> module Main
> where

> import Lexer
> import System.IO
> import System.Environment
> import Text.Parsec.Pos

> main = do
>   args <- getArgs
>   if (length args) /= 1 then do
>     putStrLn "Error: Provide one (and only one) file name as an argument."
>     return ()
>   else do
>     let fileName = head args
>     content <- readFile fileName
>     let pos = newPos fileName 1 1
>     putStrLn $ show $ lexer pos content