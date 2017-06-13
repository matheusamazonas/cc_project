> module Error where

> import Text.Parsec.Pos (SourcePos)
> import Text.Parsec.Error (errorPos, errorMessages, messageString, ParseError)

> data CompilationPhase = Lexer | Parser | TypeChecker | CodeGeneration
>   deriving (Show, Eq)
> data CompilationError = CompilationError CompilationPhase String SourcePos

> instance Show CompilationError where
>   show (CompilationError CodeGeneration m _) = "Compilation failed. " 
>             ++ "CodeGeneration Error:\n" ++ m
>   show (CompilationError ph m pos) = "Compilation failed. " 
>             ++ show ph ++ " Error:\n" ++ m ++ "\n\t at " ++ show pos 

> convertError :: CompilationPhase -> ParseError -> CompilationError
> convertError phase e = CompilationError phase (show e) (errorPos e)
