> module Lexer (lexer) where

> import Token
> import Error
> import Control.Monad (liftM2)
> import Data.Char
> import Text.Parsec.Pos

> type LexerResult = Either CompilationError [PosToken]

> (|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
> f ||| g = \x -> (f x) || (g x)

> infixr 3 <:>
> (<:>) :: PosToken -> LexerResult -> LexerResult
> a <:> b = b >>= \x -> pure $ a : x

> (<++>) :: LexerResult -> LexerResult -> LexerResult
> (<++>) = liftM2 (++)

Increments the number of lines in a SourcePos. 
Just using incSourceLine isn't enough because
every time there's a new line, we must set the
column to 1 as well.

> incLine :: SourcePos -> SourcePos
> incLine p =  setSourceColumn (incSourceLine p 1) 1

> lexer :: SourcePos -> String -> LexerResult
> lexer _ [] = pure []
> lexer p ('/':'/':xs) = lexSkipLine (incSourceColumn p 2) xs
> lexer p ('/':'*':xs) = lexSkipBlock (incSourceColumn p 2) xs
> lexer p ('\n':xs) = lexer (incLine p) xs 
> lexer p ('\t':xs) = lexer (incSourceColumn p 4) xs 
> lexer p (x:xs)    
>   | isSpace x = lexer (incSourceColumn p 1) xs
>   | isAlpha x = lexText p (x:xs)
>   | isDigit x = lexNum p (x:xs)
> lexer p ('"':s) = do
>   (lit, rest, pos) <- readLiteral ([], s, p)
>   lexStringLit p lit <++> lexer pos rest 
> lexer p ('\'':'\\':'n':'\'':xs) = (TokenChar  '\n', p)          <:> lexer (incSourceColumn p 4) xs
> lexer p ('\'':'\\':'t':'\'':xs) = (TokenChar  '\t', p)          <:> lexer (incSourceColumn p 4) xs
> lexer p ('\'':c:'\'':xs)        = (TokenChar c, p)              <:> lexer (incSourceColumn p 3) xs                 
> lexer p ('<':'=':xs)            = (TokenOp LessOrEqual, p)      <:> lexer (incSourceColumn p 2) xs
> lexer p ('>':'=':xs)            = (TokenOp GreaterOrEqual, p)   <:> lexer (incSourceColumn p 2) xs
> lexer p ('=':'=':xs)            = (TokenOp Equals, p)           <:> lexer (incSourceColumn p 2) xs
> lexer p ('!':'=':xs)            = (TokenOp Different, p)        <:> lexer (incSourceColumn p 2) xs
> lexer p ('&':'&':xs)            = (TokenOp LogicalAnd, p)       <:> lexer (incSourceColumn p 2) xs
> lexer p ('|':'|':xs)            = (TokenOp LogicalOr, p)        <:> lexer (incSourceColumn p 2) xs
> lexer p (':':':':xs)            = (TokenFuncDecl, p)            <:> lexer (incSourceColumn p 2) xs
> lexer p ('-':'>':xs)            = (TokenFuncType, p)            <:> lexer (incSourceColumn p 2) xs
> lexer p ('=':xs)                = (TokenAttribution, p)         <:> lexer (incSourceColumn p 1) xs
> lexer p ('+':xs)                = (TokenOp Plus, p)             <:> lexer (incSourceColumn p 1) xs
> lexer p ('-':xs)                = (TokenOp Minus, p)            <:> lexer (incSourceColumn p 1) xs
> lexer p ('*':xs)                = (TokenOp Times, p)            <:> lexer (incSourceColumn p 1) xs
> lexer p ('/':xs)                = (TokenOp Division, p)         <:> lexer (incSourceColumn p 1) xs
> lexer p ('%':xs)                = (TokenOp Mod, p)              <:> lexer (incSourceColumn p 1) xs
> lexer p ('(':xs)                = (TokenOpenP, p)               <:> lexer (incSourceColumn p 1) xs
> lexer p (')':xs)                = (TokenCloseP , p)             <:> lexer (incSourceColumn p 1) xs
> lexer p ('<':xs)                = (TokenOp LessThan, p)         <:> lexer (incSourceColumn p 1) xs
> lexer p ('>':xs)                = (TokenOp GreaterThan, p)      <:> lexer (incSourceColumn p 1) xs
> lexer p ('!':xs)                = (TokenOp LogicalNot, p)       <:> lexer (incSourceColumn p 1) xs
> lexer p (':':xs)                = (TokenOp ListConst, p)        <:> lexer (incSourceColumn p 1) xs
> lexer p (';':xs)                = (TokenEOL, p)                 <:> lexer (incSourceColumn p 1) xs
> lexer p ('{':xs)                = (TokenOpenCurlyB, p)          <:> lexer (incSourceColumn p 1) xs
> lexer p ('}':xs)                = (TokenCloseCurlyB, p)         <:> lexer (incSourceColumn p 1) xs
> lexer p ('[':xs)                = (TokenOpenSquareB , p)        <:> lexer (incSourceColumn p 1) xs
> lexer p (']':xs)                = (TokenCloseSquareB, p)        <:> lexer (incSourceColumn p 1) xs
> lexer p ('.':xs)                = (TokenPeriod, p)              <:> lexer (incSourceColumn p 1) xs
> lexer p (',':xs)                = (TokenComma , p)              <:> lexer (incSourceColumn p 1) xs
> lexer p (x:_)                   = Left $ CompilationError Lexer ("Can't lex: " ++ show x) p

> readLiteral :: (String, String, SourcePos) -> Either CompilationError (String, String, SourcePos)
> readLiteral (_, [], p) = Left $ CompilationError Lexer ("String literal missing closing quotes") p
> readLiteral (l, ('\\':'\"':t), p) = readLiteral (l ++ "\\\"", t, incSourceColumn p 1)
> readLiteral (l, ('\"':t), p) = Right (l, t, incSourceColumn p 1)
> readLiteral (l, (c:t), p) = readLiteral ((l++[c]), t, incSourceColumn p 1)

> listConst :: SourcePos -> PosToken
> listConst p = (TokenOp ListConst, incSourceColumn p 1)

> lexStringLit :: SourcePos -> String -> LexerResult
> lexStringLit p [] = pure [openSquareB, closeSquareB]
>   where
>     openSquareB = (TokenOpenSquareB, incSourceColumn p 2)
>     closeSquareB = (TokenCloseSquareB, incSourceColumn p 3)
> lexStringLit p ('\\':'n':cs) = (TokenChar '\n', p) <:> listConst p <:> lexStringLit p cs
> lexStringLit p ('\\':'t':cs) = (TokenChar '\t', p) <:> listConst p <:> lexStringLit p cs
> lexStringLit p ('\\':'\\':cs) = (TokenChar '\\', p) <:> listConst p <:> lexStringLit p cs
> lexStringLit p ('\\':'\"':cs) = (TokenChar '\"', p) <:> listConst p <:> lexStringLit p cs
> lexStringLit p (c:cs) = (TokenChar c, p) <:> listConst p <:> lexStringLit p cs


> lexText :: SourcePos -> String -> LexerResult
> lexText p s = lexId p id <:> lexer (incSourceColumn p (length id)) rest
>   where
>     (id, rest) = span (isAlphaNum ||| ((==) '_')) s

> lexId :: SourcePos -> String -> PosToken
> lexId p s
>   | s == "Int"      = (TokenType IntType, p)
>   | s == "Bool"     = (TokenType BoolType, p)
>   | s == "Char"     = (TokenType CharType, p)
>   | s == "Void"     = (TokenVoidType, p)
>   | s == "True"     = (TokenBool True, p)
>   | s == "False"    = (TokenBool False, p)
>   | s == "if"       = (TokenIf, p)
>   | s == "else"     = (TokenElse, p)
>   | s == "while"    = (TokenWhile, p)
>   | s == "var"      = (TokenVar, p)
>   | s == "return"   = (TokenReturn, p)
>   | s == "forall"   = (TokenForAll, p)
>   | s == "function" = (TokenFunction, p)
>   | otherwise       = (TokenId s, p)

> lexNum :: SourcePos -> String -> LexerResult
> lexNum p s = (TokenNum (read num), p) <:> lexer (incSourceColumn p (length num)) rest
>   where
>     (num, rest) = span isDigit s

> lexSkipLine :: SourcePos -> String -> LexerResult
> lexSkipLine _ []        = pure []
> lexSkipLine p ('\n':xs) = lexer (incLine p) xs
> lexSkipLine p (x:xs)    = lexSkipLine (incSourceColumn p 1) xs

> lexSkipBlock :: SourcePos -> String -> LexerResult
> lexSkipBlock _ []           = pure []
> lexSkipBlock p ('*':'/':ys) = lexer (incSourceColumn p 2) ys
> lexSkipBlock p ('\n':xs)    = lexSkipBlock (incLine p) xs
> lexSkipBlock p (x:xs)       = lexSkipBlock (incSourceColumn p 1) xs









