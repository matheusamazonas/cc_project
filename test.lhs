> import Parser 
> import Grammar
> import Lexer (lexer)
> import Text.Parsec.Combinator (eof)
> import Text.Parsec.Pos (SourcePos, newPos)
> import Text.ParserCombinators.Parsec.Prim (parse)
> import Control.Monad (void)

This parser is here because it's used just for test purposes

> pFuncDecl :: MyParser GramFuncDecl
> pFuncDecl = do
>   i <- pId
>   t <- pFuncDeclTail
>   return (GramFuncDecl i t)

> parseId       = parse (pId <* eof) "test"
> parseRetType  = parse (pRetType <* eof) "test"
> parseFunType  = parse (pFunType <* eof) "test"
> parseFTypes   = parse (pFTypes <* eof) "test"
> parseFArgs    = parse (pFArgs <* eof) "test"
> parseExpr8    = parse (pExpr8 <* eof) "test"
> parseExpr7    = parse (pExpr7 <* eof) "test"
> parseExpr6    = parse (pExpr6 <* eof) "test"
> parseExpr5    = parse (pExpr5 <* eof) "test"
> parseExpr4    = parse (pExpr4 <* eof) "test"
> parseExpr3    = parse (pExpr3 <* eof) "test"
> parseExpr2    = parse (pExpr2 <* eof) "test"
> parseExpr1    = parse (pExpr1 <* eof) "test"
> parseExpr     = parse (pExpr <* eof) "test"
> parseField    = parse (pField <* eof) "test"
> parseStmt     = parse (pStmt <* eof) "test"
> parseIf       = parse (pIf <* eof) "test"
> parseWhile    = parse (pWhile <* eof) "test"
> parseStmt1    = parse (pStmt1 <* eof) "test"
> parseReturn   = parse (pReturn <* eof) "test"
> parseFunDecl  = parse (pFuncDecl <* eof) "test"
> parseDecl     = parse (pDecl <* eof) "test"
> parseSPL      = parse (pSPL <* eof) "test"

> nP = newPos "test_file" 1 1

> testAll = do
>   putStrLn "------ RetType ------"
>   void $ testRetTypes
>   putStrLn "------ FunType ------"
>   void $ testFunTypes
>   putStrLn "------ FTypes ------"
>   void $ testFType
>   putStrLn "------ FArgs ------"
>   void $ testFArgs
>   putStrLn "------ Expr8 ------"
>   void $ testExpr8
>   putStrLn "------ Expr7 ------"
>   void $ testExpr7
>   putStrLn "------ Expr6 ------"
>   void $ testExpr6
>   putStrLn "------ Expr5 ------"
>   void $ testExpr5
>   putStrLn "------ Expr4 ------"
>   void $ testExpr4
>   putStrLn "------ Expr3 ------"
>   void $ testExpr3
>   putStrLn "------ Expr2 ------"
>   void $ testExpr2
>   putStrLn "------ Expr1 ------"
>   void $ testExpr1
>   putStrLn "------ Expr ------"
>   void $ testExpr
>   putStrLn "------ Field ------"
>   void $ testField
>   putStrLn "------ If ------"
>   void $ testIf
>   putStrLn "------ While ------"
>   void $ testWhile
>   putStrLn "------ Attrib ------"
>   void $ testAttrib
>   putStrLn "------ Return ------"
>   void $ testReturn
>   return ()	

== RetType

> retTypeProgs = ["Bool", "Char", "Int", "(Char, Int)", "Void", "[Int]", "idTest"]
> retTypeCases = map (lexer nP) retTypeProgs
> testRetTypes = putStrLn $ unlines $ map (show . parseRetType) retTypeCases

== FunType

> funTypeProgs = ["Int -> Void", "(Int, Int) -> Char", "-> Int", "Char -> Void", "[Int] -> (Bool, Char)", "-> [Bool]", "-> (Char, Int)"]
> funTypeCases = map (lexer nP) funTypeProgs
> testFunTypes = putStrLn $ unlines $ map (show . parseFunType) funTypeCases

== FTypes

> fTypeProgs = ["Bool Int", "Char", "Int Char myId Bool", "Int Int Int", "Int Int Bool", "myType"]
> fTypeCases = map (lexer nP) fTypeProgs
> testFType = putStrLn $ unlines $ map (show . parseFTypes) fTypeCases

== FArgs

> fArgsProgs = ["x", "x, y", "x, u, b", "x, g, t, we", "xdada, fsfs" ]
> fArgsCases = map (lexer nP) fArgsProgs
> testFArgs = putStrLn $ unlines $ map (show . parseFArgs) fArgsCases

== Expr8

> expr8Progs = ["my_id", "i", "i.hd", "l.tl", "k.fst", "t.snd", "t()", "e(a)", "af(a, b, c)" ]
> expr8Cases = map (lexer nP) expr8Progs
> testExpr8 = putStrLn $ unlines $ map (show . parseExpr8) expr8Cases

== Expr7

> expr7Progs = ["True", "False", "1321", "0", "-31231", "(1 + 2)", "(a * g)", "(121)", "(True)", "(a())" ]
> expr7Cases = map (lexer nP) expr7Progs
> testExpr7 = putStrLn $ unlines $ map (show . parseExpr7) expr7Cases

== Expr6

> expr6Progs = ["!True", "!a", "!5", "-True", "-a", "-232", "-a()", "-b(a,v)", "!a()" ]
> expr6Cases = map (lexer nP) expr6Progs
> testExpr6 = putStrLn $ unlines $ map (show . parseExpr6) expr6Cases

== Expr5

> expr5Progs = ["5", "5 / 6", "9 * 1", "9 % 6", "True", "True * False", "True * 9", "a * 9", "a * b", "a * b" ]
> expr5Cases = map (lexer nP) expr5Progs
> testExpr5 = putStrLn $ unlines $ map (show . parseExpr5) expr5Cases

== Expr4

> expr4Progs = ["5", "5 + 6", "9 - 1", "9 + 6", "9 * 5 - 4", "4 - 9 * 5", "7 * 9", "6 / 1", "5 % 2", "True + 2", "True + False", "True * False", "my_var", "(4) * 5", "(4 + 5) * 3", "1 + (4 * 7)" ]
> expr4Cases = map (lexer nP) expr4Progs
> testExpr4 = putStrLn $ unlines $ map (show . parseExpr4) expr4Cases

Needs nesting:
 l50 = lexer nP "5 * 2 * 6" 

== Expr3

> expr3Progs = ["5 * 2", "5 * 2 : 1", "5 * 2 : 4 + 1", "5 * 2 : 6 : 9", "5 * 2 : 1 + 5 : 7 * 1" ]
> expr3Cases = map (lexer nP) expr3Progs
> testExpr3 = putStrLn $ unlines $ map (show . parseExpr3) expr3Cases

== Expr2

> expr2Progs = ["5", "5 < 2", "6 > 1", "9 <= 1", "5 >= 3", "5 >= 3 * 2" ]
> expr2Cases = map (lexer nP) expr2Progs
> testExpr2 = putStrLn $ unlines $ map (show . parseExpr2) expr2Cases

== Expr1

> expr1Progs = ["5", "5 < 2", "True && 5 * 4 + 2", "True && False", "5 && 3", "5 && 2 + 2" ]
> expr1Cases = map (lexer nP) expr1Progs
> testExpr1 = putStrLn $ unlines $ map (show . parseExpr1) expr1Cases

== Expr

> exprProgs = ["5", "5 < 2", "True + 5 * 4 || 2", "True && False || True", "5 || 89 + 2", "5 - 2 || 12 % 12", "5 - 2 || 12 % 12 * 3 + 2", "(a, (b, c))" ]
> exprCases = map (lexer nP) exprProgs
> testExpr = putStrLn $ unlines $ map (show . parseExpr) exprCases

== Field

> fieldProgs = [".fst", ".snd", ".hd", ".tl", ".hd.tl.snd.fst" ]
> fieldCases = map (lexer nP) fieldProgs
> testField = putStrLn $ unlines $ map (show . parseField) fieldCases

== If

> ifProgs = ["if (True) {}", "if (True && False) { x = 12; }", "if (a == b) { a(); } else { a = 21; } ", "if (5 - 2 || 12 % 12) { a = 43; } else { b(); a = 45; x = 2 * 43 + True; m = a(b, 5, True); }", "if ( a(b,c,d) ) {}", "if (True) { if (4 < 5) { test (); } else {a = 4; } } else { prin(); }"]
> ifCases = map (lexer nP) ifProgs
> testIf = putStrLn $ unlines $ map (show . parseIf) ifCases

== While

> whileProgs = ["while (True) {}", "while (True && False) { x = 12; }", "while (a == b) { a(); } ", "while (5 - 2 || 12 % 12) { a = 43; }", "while ( a(b,c,d) ) {return 1 + 4;}", "while (True) { if (4 < 5) { test (); return;} }"]
> whileCases = map (lexer nP) whileProgs
> testWhile = putStrLn $ unlines $ map (show . parseWhile) whileCases

== Attrib

> attribProgs = ["a = la();", "the = fuck();", "l = 3;", "hi.snd = la(3+4, 8 < 4);", "x.tl = a(x.fst, y.snd); "]
> attribCases = map (lexer nP) attribProgs
> testAttrib = putStrLn $ unlines $ map (show . parseStmt1) attribCases

== Return

> returnlProgs = ["return;", "return 1 + 2;", "return a();", "return a + c(1,2,t);", "return c() + f(a, b, j) * 12;"]
> returnCases = map (lexer nP) returnlProgs
> testReturn = putStrLn $ unlines $ map (show . parseReturn) returnCases

== FuncDecl

> funcDecl0 = "facR (n) :: Int -> Int { return; }"
> funcDecl1 = "facR (n) { return; }"
> funcDecl2 = "facR () { return; }"
> funcDecl3 = "facR ( n ) :: Int -> Int {\nif ( n < 2 ) {\nreturn 1;\n} else {\nreturn n * facR ( n - 1 );\n}\n}"
> funcDeclProgs = [funcDecl0, funcDecl1, funcDecl2, funcDecl3]
> funcDeclCases = map (lexer nP) funcDeclProgs
> testFuncDecl = putStrLn $ unlines $ map (show . parseFunDecl) funcDeclCases

== Example1

> exampl1 = "facR ( n ) :: Int -> Int {\nif ( n < 2 ) {\nreturn 1;\n} else {\nreturn n * facR ( n - 1 );\n}\n}"
> exampl2 = "/*\nThree ways to implement the factorial function in SPL.\nFirst the recursive version.\n*/\nfacR ( n ) :: Int -> Int {\nif ( n < 2 ) {\nreturn 1;\n} else {\nreturn n * facR ( n - 1 );\n}\n}"
> examples = [exampl1, exampl2]
> exampl1Case = map (lexer nP) examples
> testExample1 = putStrLn $ unlines $ map (show . parseSPL) exampl1Case
