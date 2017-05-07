> module Dependency (dependencyBlocks) where

> import Data.Graph
> import Grammar

===============================================================================
===============================================================================
Dependency

Performs dependency analysis on a program.
This splits the parse tree (list of variable and function declarations) into 
blocks of declarations. Each block is a strongly connected component in 
the constructed dependency graph; this means they are mutually recursive.
Furthermore, blocks are topologically sorted; this means that, regardless of 
the definition order in the user's program, variables and functions are 
defined before they are used (outside of their own mutually recursive block).

Notes:
Called with dependencyBlocks.
Does not support higher-order functions.
===============================================================================
===============================================================================


> dependencyBlocks :: [GramDecl] -> [[GramDecl]]
> dependencyBlocks = map unpackSCC . stronglyConnComp . dependencies
>   where unpackSCC (AcyclicSCC el) = [el]
>         unpackSCC (CyclicSCC els) = els

> dependencies :: [GramDecl] -> [(GramDecl, String, [String])]
> dependencies [] = []
> dependencies (decl:decls) = declDeps decl : dependencies decls

> declDeps :: GramDecl -> (GramDecl, String, [String])
> declDeps (GramDeclVar vardecl) = (GramDeclVar vardecl, varname, deps)
>   where (varname, deps) = varDeps vardecl
> declDeps (GramDeclFun funcdecl) = (GramDeclFun funcdecl, funcname, funcDeps funcdecltail)
>   where (GramFuncDecl (Id _ funcname) funcdecltail) = funcdecl

> varDeps :: GramVarDecl -> (String, [String])
> varDeps (GramVarDeclVar    (GramVarDeclTail (Id _ varname) e)) = (varname, exprDeps [] e)
> varDeps (GramVarDeclType _ (GramVarDeclTail (Id _ varname) e)) = (varname, exprDeps [] e)

> funcDeps :: GramFuncDeclTail -> [String]
> funcDeps (GramFuncDeclTail fargs _ stmts) = blockDeps (argList fargs) stmts
>   where argList [] = []
>         argList [GramFArgsId (Id _ argname) fargs] = argname : argList fargs

> blockDeps :: [String] -> [GramStmt] -> [String]
> blockDeps _ [] = []
> blockDeps locals (stmt:stmts) = deps ++ (blockDeps newlocals stmts)
>   where (newlocals, deps) = stmtDeps locals stmt

> stmtDeps :: [String] -> GramStmt -> ([String], [String])
> stmtDeps locals (GramIf _ e tr fa) = ([], (exprDeps locals e) ++ (blockDeps locals tr) ++ (blockDeps locals fa))
> stmtDeps locals (GramWhile _ e loop) = ([], (exprDeps locals e) ++ (blockDeps locals loop))
> stmtDeps locals (GramAttr _ (Var (Id _ varname) _) e)
>   | varname `elem` locals = ([], exprDeps locals e)
>   | otherwise = ([], varname : exprDeps locals e)
> stmtDeps locals (GramStmtFunCall (GramFunCall (Id _ varname) args)) = ([], varname : argDeps locals args) -- this part needs to be added to for HOF
>   where argDeps _ [] = []
>         argDeps locals (e:args) = (exprDeps locals e) ++ (argDeps locals args)
> stmtDeps locals (GramReturn _ me) = 
>   case me of Just e  -> ([], exprDeps locals e)
>              Nothing -> ([],[])
> stmtDeps locals (GramFunVarDecl vardecl) = ([varname], deps)
>   where (varname, deps) = varDeps vardecl

> exprDeps :: [String] -> GramExp -> [String]
> exprDeps locals (GramExpTuple _ e1 e2) = (exprDeps locals e1) ++ (exprDeps locals e2)
> exprDeps locals (GramBinary _ _ e1 e2) = (exprDeps locals e1) ++ (exprDeps locals e2)
> exprDeps locals (GramUnary _ _ e) = exprDeps locals e
> exprDeps locals (GramExpId (Var (Id _ varname) _))
>   | varname `elem` locals = []
>   | otherwise = [varname]
> exprDeps locals (GramExpFunCall (GramFunCall (Id _ varname) args))
>   | varname `elem` locals = argDeps locals args -- this part needs to be added to for HOF
>   | otherwise = varname : argDeps locals args
>   where argDeps _ [] = []
>         argDeps locals (e:args) = (exprDeps locals e) ++ (argDeps locals args)
> exprDeps _ _ = []