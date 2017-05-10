> module Test where

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

> expr5Progs = ["5", "5 / 6", "9 * 1", "9 % 6", "True", "True * False", "True * 9", "a * 9", "a * b", "a * b", "5 * 4 * 3 * 2", "1 / 2 / 3 / 4" ]
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

> example1 = "facR ( n ) :: Int -> Int {\nif ( n < 2 ) {\nreturn 1;\n} else {\nreturn n * facR ( n - 1 );\n}\n}"
> example2 = "// The iterative version of the factorial function\nfacI ( n ) :: Int -> Int {\nvar r = 1;\nwhile ( n > 1 ) {\nr = r * n;\nn = n - 1;\n}\nreturn r;\n}"
> example3 = "/// A main function to check the results\n// It takes no arguments, so the type looks like this:\nmain ( ) :: -> Void {\nvar n = 0;\nvar facN = 1;\nvar ok = True;\n	\nwhile ( n < 20 ) {\nfacN = facR ( n );\nif ( facN != facI ( n ) || facN != facL ( n )) {\nprint ( n : facN : facI ( n ) : facL ( n ) : [] );\nok = False;\n}\nn = n + 1;\n}\nprint ( ok );\n}"
> example4 = "// A list based factorial function\n// Defined here to show that functions can be given in any order (unlike C)\nfacL ( n ) :: Int -> Int {\nreturn product (fromTo ( 1, n ) );\n}"
> example5 = "// Computes the product of a list of integers\nproduct ( list ) :: [Int] -> Int {\n	if ( isEmpty ( list ) ) {\n		return 1;\n	} else {\n		return list.hd * product ( list.tl );\n	}\n}"
> example6 = "// Generates a list of integers from the first to the last argument\nfromTo ( from, to ) :: Int Int -> [ Int ] {\n	if ( from <= to ) {\n		return from : fromTo ( from + 1, to );\n	} else {\n		return [] ;\n	}\n}"
> example7 = "// Make a reversed copy of any list\nreverse ( list ) :: [ t ] -> [ t ] {\n	var accu = [];\n	while ( ! isEmpty ( list ) ) {\n		accu = list.hd : accu ;\n		list = list.tl;\n	}\n	return accu ;\n}"
> example8 = "// Absolute value, in a strange layout\nabs ( n ) :: Int -> Int { if (n < 0) { return -n; } else { return n ; } }"
> example9 = "// make a copy of a tuple with swapped elements\nswapCopy ( pair ) :: (a, b) -> (b, a) {\n	return ( pair. snd, pair.fst );\n}"	
> example10 = "// swap the elements in a tuple\nswap ( tuple ) :: (a, a) -> (a, a) {\n	var tmp = tuple.fst ;\n	tuple.fst = tuple.snd;\n	tuple.snd = tmp;\n	return tuple;\n}"	
> example11 = "// list append\nappend ( l1 , l2 ) :: [t] [t] -> [t] {\n	if ( isEmpty ( l1 ) ) {\n		return l2 ;\n	} else {\n		l1.tl = append ( l1.tl, l2 );\n		return l1;\n	}\n}"
> example12 = "// square the odd numbers in a list and remove the even numbers\nsquareOddNumbers ( list ) :: [Int] -> [Int] {\n	while ( ! isEmpty ( list ) && list.hd % 2 == 0) {\n		list = list.tl ;\n	}\n	if ( ! isEmpty (list) ) {\n		list.hd = list.hd * list.hd;\n		list.tl = squareOddNumbers( list.tl );\n	}\n	return list ;\n}"
> examples = [example1, example2, example3, example4, example5, example6, example7, example8, example9, example10, example11, example12]
> exampl1Case = lexer nP example1
> testExample1 = putStrLn $ show $ parseSPL exampl1Case
> exampl2Case = lexer nP example2
> testExample2 = putStrLn $ show $ parseSPL exampl2Case
> exampl3Case = lexer nP example3
> testExample3 = putStrLn $ show $ parseSPL exampl3Case
> exampl4Case = lexer nP example4
> testExample4 = putStrLn $ show $ parseSPL exampl4Case
> exampl5Case = lexer nP example5
> testExample5 = putStrLn $ show $ parseSPL exampl5Case
> exampl6Case = lexer nP example6
> testExample6 = putStrLn $ show $ parseSPL exampl6Case
> exampl7Case = lexer nP example7
> testExample7 = putStrLn $ show $ parseSPL exampl7Case
> exampl8Case = lexer nP example8
> testExample8 = putStrLn $ show $ parseSPL exampl8Case
> exampl9Case = lexer nP example9
> testExample9 = putStrLn $ show $ parseSPL exampl9Case
> exampl10Case = lexer nP example10
> testExample10 = putStrLn $ show $ parseSPL exampl10Case
> exampl11Case = lexer nP example11
> testExample11 = putStrLn $ show $ parseSPL exampl11Case
> exampl12Case = lexer nP example12
> testExample12 = putStrLn $ show $ parseSPL exampl12Case

