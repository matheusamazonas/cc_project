> module Main where

> import Lexer (lexer)
> import Parser (parseTokens)
> import TypeChecker (inferProgT)
> import Generator (generate)
> import System.IO
> import System.Environment
> import Text.Parsec.Pos (newPos)

> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args) /= 1 then do
>     putStrLn "Error: Provide one (and only one) file name as an argument."
>     return ()
>   else do
>     let sourceName = head args
>     content <- readFile sourceName
>     let pos = newPos sourceName 1 1
>     let ast = parseTokens sourceName $ lexer pos content
>     case ast of
>       Left e -> putStrLn $ show e
>       Right ast -> do
>         let i = inferProgT ast
>         case i of
>           Left e -> putStrLn $ show e
>           Right _ -> do
>             let code = generate ast
>             writeFile "output.ssm" code
