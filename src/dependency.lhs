> module Dependency (dependencyAnalysis, Capture) where

> import Data.Graph
> import Data.List ((\\))
> import Grammar

> type FunId = String
> type VarId = String
> data Capture = Capture GramId [VarId] [Capture]

===============================================================================
===============================================================================
This script performs the following two closely related functions:

Global dependency analysis

Performs global dependency analysis on a program.
This splits the parse tree (list of variable and function declarations) into 
blocks of declarations. Each block is a strongly connected component in 
the constructed dependency graph; this means they are mutually recursive.
Furthermore, blocks are topologically sorted; this means that, regardless of 
the definition order in the user's program, variables and functions are 
defined before they are used (outside of their own mutually recursive block).

Capture analysis

Performs capture analysis on a function.
This constructs a recursive structure listing local variables that are 
captured by nested functions. The main function whose declaration is 
traversed can use this to decide which variables are stored on the heap.
The nested functions can use this to know the number and order of 
variables in their environment, as this depends on both themselves and 
nested functions.
===============================================================================
===============================================================================


> dependencyAnalysis :: [GramDecl] -> ([[GramDecl]], [Capture])
> dependencyAnalysis decls = (map unpackSCC $ stronglyConnComp deps, capts)
>   where globals = getGlobals decls
>         (deps, capts) = dependencies globals decls
>         unpackSCC (AcyclicSCC el) = [el]
>         unpackSCC (CyclicSCC els) = els

> dependencies :: [VarId] -> [GramDecl] -> ([(GramDecl, VarId, [VarId])], [Capture])
> dependencies _ [] = ([],[])
> dependencies globals (decl:decls) = (deps ++ otherdeps, capts ++ othercapts)
>   where (deps, capts) = declDeps globals decl
>         (otherdeps, othercapts) = dependencies globals decls

> declDeps :: [VarId] -> GramDecl -> ((GramDecl, VarId, [VarId]), [Capture])
> declDeps _ (GramDeclVar vardecl) = ((GramDeclVar vardecl, varname, deps), [])
>   where (varname, deps) = varDeclDeps [] vardecl
> declDeps globals (GramDeclFun fundecl) = ((GramDeclFun fundecl, fid, deps), [capt])
>   where (GramFuncDecl fun@(Id _ fid) args _ stmts) = fDecl
>         (capt, deps) = funcDeps globals fun args stmts

> funcDeps :: [VarId] -> GramId -> [GramId] -> [GramStmt] -> (Capture, [VarId])
> funcDeps globals fun@(Id _ fid) fargs stmts = (capt, deps)
>   where listArgs = map (\(Id _ argid) -> argid)
>         (nestedcapts, deps) = blockDeps (fid : listArgs fargs) stmts
>         capt = Capture fun (deps \\ globals) nestedcapts -- global variables/functions are not captured

> varDeclDeps :: [VarId] -> GramVarDecl -> (VarId, [VarId])
> varDeclDeps locals (GramVarDeclVar    (Id _ vid) e) = (vid, exprDeps (vid:locals) e) -- recursive variable definitions are prohibited by the type checker
> varDeclDeps locals (GramVarDeclType _ (Id _ vid) e) = (vid, exprDeps (vid:locals) e) -- but still do not need to affect dependencies

> blockDeps :: [VarId] -> [VarId] -> [GramStmt] -> ([Capture], [VarId])
> blockDeps _ _ [] = ([],[])
> blockDeps globals locals (stmt:stmts) = (capts ++ newcapts, deps ++ blockdeps)
>   where (newlocals, capts, deps) = stmtDeps locals stmt
>         (newcapts, blockdeps) = blockDeps globals (locals ++ newlocals) stmts

> stmtDeps :: [VarId] -> [VarId] -> GramStmt -> ([VarId], [Capture], [VarId])
> stmtDeps globals locals (GramIf _ e tr fa) = ([], trcapts ++ facapts, (exprDeps locals e) ++ trdeps ++ fadeps)
>   where (trcapts, trdeps) = blockDeps globals locals tr
>         (facapts, fadeps) = blockDeps globals locals fa
> stmtDeps globals locals (GramWhile _ e loop) = ([], capts, (exprDeps locals e) ++ deps)
>   where (capts, deps) = blockDeps globals locals loop
> stmtDeps globals locals (GramAttr _ (Var (Id _ vid) _) e)
>   | vid `elem` locals = ([], exprDeps locals e)
>   | otherwise = ([], vid : exprDeps locals e)
> stmtDeps globals locals (GramStmtFunCall funcall) = ([], funCallDeps locals funcall)
> stmtDeps globals locals (GramReturn _ me) = 
>   case me of Just e  -> ([], exprDeps locals e)
>              Nothing -> ([],[])
> stmtDeps globals locals (GramFunVarDecl vardecl) = ([varname], deps)
>   where (varname, deps) = varDeclDeps locals vardecl
> stmtDeps globals locals (GramStmtFuncDecl fundecl) = ([funname], [capt], deps \\ locals) -- locals are not global dependencies, but
>   where (GramFuncDecl fid args _ stmts) = fundecl                                        -- for capture analysis, their usage should
>         (capt, deps) = funcDeps globals fid args stmts                                   -- not be ignored in the nested function

> exprDeps :: [VarId] -> GramExp -> [VarId]
> exprDeps locals (GramExpTuple _ e1 e2) = (exprDeps locals e1) ++ (exprDeps locals e2)
> exprDeps locals (GramBinary _ _ e1 e2) = (exprDeps locals e1) ++ (exprDeps locals e2)
> exprDeps locals (GramUnary _ _ e) = exprDeps locals e
> exprDeps locals (GramExpId (Var (Id _ vid) _))
>   | vid `elem` locals = []
>   | otherwise = [vid]
> exprDeps locals (GramExpFunCall funcall) = funCallDeps locals funcall
> exprDeps _ _ = []

> funCallDeps :: [VarId] -> GramFunCall -> [VarId]
> funCallDeps locals (GramFunCall (Id _ fid) args)
>   | fid `elem` locals = argDeps locals args
>   | otherwise = fid : argDeps locals args
>   where argDeps locals = concat . map (exprDeps locals)

> getGlobals :: [GramDecl] -> [VarId]
> getGlobals = map getName
>   where getName (GramDeclFun (GramFuncDecl (Id _ fid) _ _ _))  = fid
>         getName (GramDeclVar (GramVarDeclType _ (Id _ vid) _)) = vid
>         getName (GramDeclVar (GramVarDeclVar    (Id _ vid) _)) = vid