> module Main where

> import Dependency (dependencyAnalysis)
> import Lexer (lexer)
> import Parser (parseTokens)
> import PolyChecker (inferProg)
> import Generator (generate)
> import System.IO
> import System.Environment
> import System.Process (system)
> import Text.Parsec.Pos (newPos)
> import System.FilePath.Posix (dropExtension)
> import GHC.IO.Exception (ExitCode)

> usage = "Usage: main [mode] [source path]\nModes:\n\tc (compile)\n\tm (make)\n\tx (execute)\n\td (debug)"

> execute :: FilePath -> IO ExitCode
> execute fileName = system $ "java -jar ../ssm/ssm.jar --cli --file " ++ fileName

> compile :: FilePath -> FilePath -> IO String
> compile sourceName ssmName = do
>   content <- readFile sourceName
>   let pos = newPos sourceName 1 1
>   let ast = parseTokens sourceName $ lexer pos content
>   case ast of
>     Left e -> do putStrLn $ show e; return ""
>     Right ast -> do
>       let (astBlocks, captures) = dependencyAnalysis ast
>       let i = inferProg astBlocks
>       case i of
>         Left e -> do putStrLn $ show e; return ""
>         Right aast -> do
>           return $ generate captures aast

> main :: IO ()
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
>         source <- compile sourceName ssmName
>         putStrLn $ "Code:\n" ++ source
>         putStrLn source
>       "m" -> do
>         source <- compile sourceName ssmName
>         writeFile ssmName source
>       "x" -> do
>         source <- compile sourceName ssmName
>         writeFile ssmName source
>         putStrLn $ "Execution:" 
>         execute ssmName
>         return ()
>       "d" -> do
>         source <- compile sourceName ssmName
>         putStrLn $ "Code:\n" ++ source
>         writeFile ssmName source
>         putStrLn $ "Execution:" 
>         execute ssmName
>         return ()
>       otherwise -> putStrLn $ "Invalid mode\n" ++ usage


