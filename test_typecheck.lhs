> module TestTypeCheck where

> import Test
> import TypeChecker
> import Lexer (lexer)

> newEnv = ([], [[]], 1)

--------------------- Suite ---------------------

> countSuccTests :: (Show a) => [Either a b] -> [a]
> countSuccTests [] = []
> countSuccTests ((Right _):ts) = countSuccTests ts
> countSuccTests ((Left e) :ts) = e: (countSuccTests ts)

> parseTests p cases = map (p . lexer nP) cases

> test :: (Show a) => [Either a b] -> String -> IO ()
> test tests s = do
>   let l = length tests
>   putStr $ "Running " ++ show l ++ s ++ " tests... "
>   let fails = countSuccTests tests
>   putStrLn $ show (l - length fails) ++ s ++ " tests succeeded."
>   putStrLn $ "Failed tests: " ++ show fails

> testStmts :: IO ()
> testStmts = test checkStmts " Stmt"

> testFunDecls :: IO ()
> testFunDecls = test checkFunDecls " FunDecl"

> testAllCheck :: IO ()
> testAllCheck = do
>   testStmts
>   testFunDecls

--------------------- Stmts ---------------------

> checkStmt eTree = case eTree of
>   Right ast -> inferStmtT newEnv ast TInt
>   Left e -> error $ "Couldn't parse" ++ show e

> checkStmts = map checkStmt $ parseTests parseStmt stmtTests

> stmtTest1 = "if (True) {}"
> stmtTest2 = "if (True) {} else {}"
> stmtTest3 = "if (True) { return 1; } else { return 2; }"
> stmtTest4 = "if(True) { var x = 5; var z = x+1; return z; } else { return 1; }"
> stmtTest5 = "if (True) { return 1; }"
> stmtTest6 = "while (True) {} "
> stmtTest7 = "while (4<8) { var x = 7; var y = 3; }"
> stmtTest8 = "if(8<=4) { while (True) { var g = 4; } }"
> stmtTest9 = "while (True && False) { if (4<8) {var m = 7;} else {var l = 1; } } "
> stmtTest10 = "while (True) { while (4 < 6) { while (9>1) {} } }"
> stmtTests = [stmtTest1, stmtTest2, stmtTest3, stmtTest4, stmtTest5, stmtTest6, stmtTest7, stmtTest8, stmtTest9, stmtTest10]


--------------------- Function declaration ---------------------

> checkFunDecl eTree = case eTree of
>   Right ast -> inferDeclT newEnv ast 
>   Left e -> error $ "Couldn't parse" ++ show e


> checkFunDecls = map checkFunDecl $ parseTests parseDecl funDeclTests

> funDeclTest1 = "foo() {}"
> funDeclTest2 = "foo() { return 1; }"
> funDeclTest3 = "foo() :: -> Int { return 1; }"
> funDeclTest4 = "foo(x) { return x+1; }"
> funDeclTest5 = "foo(x) :: Int -> Int { return x+1; }"
> funDeclTest6 = "foo(x) { return x; return 1; }"
> funDeclTest7 = "foo(x,y,z) { return x:y:z:1:[]; }"
> funDeclTest8 = "foo(x) :: Int -> Int { if (1<2) { return x; } else { return 1; } }"
> funDeclTest9 = "foo(x, y) :: Int Int -> Int { if (x<y) { return x; } else { return y; } }"
> funDeclTest10 = "foo(x, y) { if (x<y) { return x; } else { return y; } }"
> funDeclTests = [funDeclTest1, funDeclTest2, funDeclTest3, funDeclTest4, funDeclTest5, funDeclTest6, funDeclTest7, funDeclTest8, funDeclTest9, funDeclTest10]





