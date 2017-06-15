> module Error where

> import Text.Parsec.Pos (SourcePos)
> import Text.Parsec.Error (errorPos, errorMessages, messageString, ParseError)

> data CompilationPhase = Lexer | Parser | TypeChecker | CodeGeneration
>   deriving (Eq)

> instance Show CompilationPhase where
>   show Lexer = "Lexer"
>   show Parser = "Parser"
>   show TypeChecker = "Type checking"
>   show CodeGeneration = "Code generation"

> data CompilationError = CompilationError CompilationPhase String SourcePos

> instance Show CompilationError where
>   show (CompilationError CodeGeneration m _) = "Compilation failed. " 
>             ++ "Code generation error:\n" ++ m
>   show (CompilationError ph m pos) = "Compilation failed. " 
>             ++ show ph ++ " error:\n" ++ m ++ "\nat " ++ show pos 

> convertError :: CompilationPhase -> ParseError -> CompilationError
> convertError phase e = CompilationError phase (show e) (errorPos e)
