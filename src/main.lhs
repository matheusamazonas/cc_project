> module Main where

> import Error (CompilationError)
> import Dependency (dependencyAnalysis)
> import Lexer (lexer)
> import Parser (parseTokens)
> import Printer (printGram)
> import PolyChecker (inferProg)
> import Generator (generate)
> import System.IO
> import System.Environment
> import System.Process (system)
> import Text.Parsec.Pos (newPos)
> import System.FilePath.Posix (dropExtension)
> import GHC.IO.Exception (ExitCode)

> usage = "Usage: main [mode] [source path]\nModes:\n\tc (compile)\n\tm (make)\n\tx (execute)\n\td (debug)"

> runExecutable :: FilePath -> IO ExitCode
> runExecutable fileName = system $ "java -jar ../ssm/ssm.jar --cli --file " ++ fileName

> compileFile :: String -> String -> Either CompilationError String
> compileFile sourceName content = do
>   let pos = newPos sourceName 1 1
>   tokens <- lexer pos content
>   tree <- parseTokens sourceName tokens 
>   let (blocks, _) = dependencyAnalysis tree
>   aast <- inferProg blocks
>   let (_, captures) = dependencyAnalysis aast
>   result <- generate captures aast
>   return result

> compile :: String -> String -> String -> IO ()
> compile ssmName sourceName code = do
>   putStrLn $ "Code:\n" ++ code
>   putStrLn code

> make :: String -> String -> String -> IO ()
> make ssmName sourceName code = do
>   writeFile ssmName code

> execute :: String -> String -> String -> IO ()
> execute ssmName sourceName code = do
>   writeFile ssmName code
>   putStrLn $ "Execution:" 
>   runExecutable ssmName
>   return ()

> debug :: String -> String -> String -> IO ()
> debug ssmName sourceName code = do
>   putStrLn $ "Code:\n" ++ code
>   writeFile ssmName code
>   putStrLn $ "Execution:" 
>   runExecutable ssmName
>   return ()

> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args) /= 2 then do
>     putStrLn $ "Error: Insufficient arguments.\n" ++ usage
>   else do
>     let mode = head args
>     let sourceName = args !! 1
>     let ssmName = (dropExtension sourceName) ++ ".ssm"
>     content <- readFile sourceName
>     case compileFile sourceName content of
>       Left e -> putStrLn $ show e
>       Right code -> do
>         case mode of
>           "c" -> compile ssmName sourceName code
>           "m" -> make ssmName sourceName code
>           "x" -> execute ssmName sourceName code
>           "d" -> debug ssmName sourceName code
>           otherwise -> putStrLn $ "Invalid mode\n" ++ usage


