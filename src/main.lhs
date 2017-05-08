> module Main where

> import Lexer (lexer)
> import Parser (parseTokens)
> import TypeChecker (inferProgT)
> import Generator (generate)
> import System.IO
> import System.Environment
> import System.Process (createProcess, proc, ProcessHandle)
> import Text.Parsec.Pos (newPos)
> import System.FilePath.Posix (dropExtension)

> usage = "Usage: main [mode] [source path]\nModes: \n\tc (compile) \n\tx (compile and execute)"

> execute :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
> execute fileName = createProcess (proc "java" ["-jar", "../ssm/ssm.jar", "--cli", "--file", fileName]) 

> compile :: FilePath -> FilePath -> IO ()
> compile sourceName ssmName = do
>   content <- readFile sourceName
>   let pos = newPos sourceName 1 1
>   let ast = parseTokens sourceName $ lexer pos content
>   case ast of
>     Left e -> putStrLn $ show e
>     Right ast -> do
>       let i = inferProgT ast
>       case i of
>         Left e -> putStrLn $ show e
>         Right _ -> do
>           let code = generate ast
>           writeFile ssmName code


> main = do
>   args <- getArgs
>   if (length args) /= 2 then do
>     putStrLn $ "Error: Insufficient arguments.\n" ++ usage
>   else do
>     let mode = head args
>     let sourceName = args !! 1
>     let ssmName = (dropExtension sourceName) ++ ".ssm"
>     case mode of
>       "c" -> do
>         compile sourceName ssmName
>       "x" -> do
>         compile sourceName ssmName
>         (_, x, _, _) <- execute ssmName
>         putStrLn $ show x
>       otherwise -> putStrLn $ "Invalid mode\n" ++ usage


