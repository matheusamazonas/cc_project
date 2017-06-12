> module Main where

> import Dependency (dependencyAnalysis)
> import Lexer (lexer)
> import Parser (parseTokens, showParsingErrors)
> import PolyChecker (inferProg)
> import Generator (generate)
> import System.IO
> import System.Environment
> import System.Process (system)
> import Text.Parsec.Pos (newPos)
> import System.FilePath.Posix (dropExtension)
> import Text.Parsec.Error (errorPos, errorMessages)
> import GHC.IO.Exception (ExitCode)

> usage = "Usage: main [mode] [source path]\nModes:\n\tc (compile)\n\tm (make)\n\tx (execute)\n\td (debug)"

> runExecutable :: FilePath -> IO ExitCode
> runExecutable fileName = system $ "java -jar ../ssm/ssm.jar --cli --file " ++ fileName

> compileFile :: FilePath -> FilePath -> IO (Maybe String)
> compileFile sourceName ssmName = do
>   content <- readFile sourceName
>   let pos = newPos sourceName 1 1
>   case lexer pos content of
>     Left (e, p) -> do 
>       putStrLn $ "[LEXER ERROR] " ++ e ++ "\n\tat " ++ show p
>       return Nothing
>     Right ts -> do
>       case parseTokens sourceName ts of
>         Left e -> do 
>           putStrLn $ "[PARSER ERROR] " ++ showParsingErrors (errorMessages e)
>           putStrLn $ "\tat " ++ show (errorPos e)
>           return Nothing
>         Right ast -> do
>           let (astBlocks, captures) = dependencyAnalysis ast
>           case inferProg astBlocks of
>             Left (e, p) -> do 
>               putStrLn $ "[TYPE ERROR] " ++ e ++ "\n\tat " ++ show p
>               return Nothing
>             Right aast -> do
>               case generate captures aast of
>                 Left e -> do 
>                   putStrLn $ "[GENERATOR ERROR] " ++ e
>                   return Nothing
>                 Right code -> return $ Just code

> compile :: String -> String -> Maybe String -> IO ()
> compile ssmName sourceName result = do
>   case result of
>     Just code -> do
>       putStrLn $ "Code:\n" ++ code
>       putStrLn code
>     Nothing -> return ()

> make :: String -> String -> Maybe String -> IO ()
> make ssmName sourceName result = do
>   case result of
>     Just code -> do
>       writeFile ssmName code
>     Nothing -> return ()

> execute :: String -> String -> Maybe String -> IO ()
> execute ssmName sourceName result = do
>   case result of
>     Just code -> do
>       writeFile ssmName code
>       putStrLn $ "Execution:" 
>       runExecutable ssmName
>       return ()
>     Nothing -> return ()

> debug :: String -> String -> Maybe String -> IO ()
> debug ssmName sourceName result = do
>   case result of
>     Just code -> do
>       putStrLn $ "Code:\n" ++ code
>       writeFile ssmName code
>       putStrLn $ "Execution:" 
>       runExecutable ssmName
>       return ()
>     Nothing -> return ()

> main :: IO ()
> main = do
>   args <- getArgs
>   if (length args) /= 2 then do
>     putStrLn $ "Error: Insufficient arguments.\n" ++ usage
>   else do
>     let mode = head args
>     let sourceName = args !! 1
>     let ssmName = (dropExtension sourceName) ++ ".ssm"
>     compResult <- compileFile sourceName ssmName
>     case mode of
>       "c" -> compile ssmName sourceName compResult
>       "m" -> make ssmName sourceName compResult
>       "x" -> execute ssmName sourceName compResult
>       "d" -> debug ssmName sourceName compResult
>       otherwise -> putStrLn $ "Invalid mode\n" ++ usage


