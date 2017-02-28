> module Main
> where

> import Lexer
> import System.IO
> import System.Environment

> main = do
>   args <- getArgs
>   if (length args) /= 1 then do
>     putStrLn "Error: Provide one (and only one) file name as an argument."
>     return ()
>   else do
>     let fileName = head args
>     content <- readFile fileName
>     putStrLn $ show $ lexer $ content