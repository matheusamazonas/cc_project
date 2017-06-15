> module Dependency (dependencyAnalysis, freeTypeVars, Capture(..)) where

> import Data.Graph
> import Data.List ((\\), find, isPrefixOf, nub)
> import Grammar
> import Text.Parsec.Pos (newPos)

> type VariableScopes = [[[GramId]]] -- three levels deep to be able to distinguish between within-function blocks and nested functions
> data Capture = Capture GramId [GramId] [Capture] -- Capture FunctionName NamesOfVariablesCapturedByMe NestedFunctionCaptures
>   deriving (Show)

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
nested functions. They are named in terms of GramIds at definition time,
so that they can be matched independently of nesting and branching depth.
===============================================================================
===============================================================================


> dependencyAnalysis :: [GramDecl] -> ([[GramDecl]], [Capture])
> dependencyAnalysis decls = (map unpackSCC $ stronglyConnComp deps, capts)
>   where initScope = [[getGlobals decls]]
>         (deps, capts) = dependencies initScope decls
>         unpackSCC (AcyclicSCC el) = [el]
>         unpackSCC (CyclicSCC els) = els

> dependencies :: VariableScopes -> [GramDecl] -> ([(GramDecl, GramId, [GramId])], [Capture])
> dependencies _ [] = ([],[])
> dependencies defs (decl:decls) = (deps:otherdeps, capts ++ othercapts)
>   where (deps, capts) = declDeps defs decl
>         (otherdeps, othercapts) = dependencies defs decls

> declDeps :: VariableScopes -> GramDecl -> ((GramDecl, GramId, [GramId]), [Capture])
> declDeps defs (GramDeclVar vardecl) = ((GramDeclVar vardecl, varname, deps), [])
>   where (varname, deps) = varDeclDeps (newFunc defs []) vardecl -- new scope needed so globals are not treated as locals
> declDeps defs (GramDeclFun fundecl) = ((GramDeclFun fundecl, fid, deps), [capt])
>   where (GramFuncDecl fid args annot stmts) = fundecl
>         (capt, deps) = funcDeps defs fid args annot stmts

> funcDeps :: VariableScopes -> GramId -> [GramId] -> [GramFunTypeAnnot] -> [GramStmt] -> (Capture, [GramId])
> funcDeps defs fid fargs annot stmts = (capt, nub deps)
>   where funlocals = (fid:fargs) ++ (typeAnnotDeps defs annot)
>         newdefs = newFunc defs funlocals
>         (nestedcapts, deps) = blockDeps newdefs stmts
>         capturedvars = (nub deps) \\ (globalScope defs) -- global variables/functions are not captured.
>         capt = Capture fid capturedvars nestedcapts     -- note local variables of this function are not in deps

> varDeclDeps :: VariableScopes -> GramVarDecl -> (GramId, [GramId])
> varDeclDeps defs (GramVarDeclVar    vid e) = (vid, exprDeps defs e)
> varDeclDeps defs (GramVarDeclType _ vid e) = (vid, exprDeps defs e)

> blockDeps :: VariableScopes -> [GramStmt] -> ([Capture], [GramId])
> blockDeps _ [] = ([],[])
> blockDeps defs (stmt:stmts) = (newcapts ++ blockcapts, deps ++ blockdeps)
>   where (newlocals, newcapts, deps) = stmtDeps defs stmt
>         (blockcapts, blockdeps) = blockDeps (addToScope defs newlocals) stmts

> stmtDeps :: VariableScopes -> GramStmt -> ([GramId], [Capture], [GramId])
> stmtDeps defs (GramIf _ e tr fa) = ([], trcapts ++ facapts, (exprDeps defs e) ++ trdeps ++ fadeps)
>   where (trcapts, trdeps) = blockDeps (newBlock defs) tr
>         (facapts, fadeps) = blockDeps (newBlock defs) fa
> stmtDeps defs (GramWhile _ e loop) = ([], capts, (exprDeps defs e) ++ deps)
>   where (capts, deps) = blockDeps (newBlock defs) loop
> stmtDeps defs (GramAttr _ (Var vid _) e) = ([],[], (varDependency defs vid) ++ (exprDeps defs e))
> stmtDeps defs (GramStmtFunCall funcall) = ([],[], funCallDeps defs funcall)
> stmtDeps defs (GramReturn _ me) = 
>   case me of Just e  -> ([],[], exprDeps defs e)
>              Nothing -> ([],[],[])
> stmtDeps defs (GramFunVarDecl vardecl) = ([varname], [], deps)
>   where (varname, deps) = varDeclDeps defs vardecl
> stmtDeps defs (GramStmtFuncDecl fundecl) = ([fid], [capt], nonlocaldeps)
>   where (GramFuncDecl fid args annot stmts) = fundecl                
>         (capt, deps) = funcDeps defs fid args annot stmts                       
>         nonlocaldeps = concat $ map (varDependency defs) deps -- removes nested function's dependencies of local variables within this function scope

> exprDeps :: VariableScopes -> GramExp -> [GramId]
> exprDeps defs (GramExpTuple _ e1 e2) = (exprDeps defs e1) ++ (exprDeps defs e2)
> exprDeps defs (GramBinary _ _ e1 e2) = (exprDeps defs e1) ++ (exprDeps defs e2)
> exprDeps defs (GramUnary _ _ e) = exprDeps defs e
> exprDeps defs (GramExpId (Var vid _)) = varDependency defs vid
> exprDeps defs (GramExpFunCall funcall) = funCallDeps defs funcall
> exprDeps _ _ = []

> funCallDeps :: VariableScopes -> GramFunCall -> [GramId]
> funCallDeps defs (GramOverloadedFunCall annots fid args) = (typesDeps defs annots) ++ (funCallDeps defs $ GramFunCall fid args)
> funCallDeps defs (GramFunCall fid args) = (varDependency defs fid) ++ (argDeps defs args)
>   where argDeps defs = concat . map (exprDeps defs)

> typeAnnotDeps :: VariableScopes -> [GramFunTypeAnnot] -> [GramId]
> typeAnnotDeps _ [] = []
> typeAnnotDeps defs [GramFunTypeAnnot ts _] = typesDeps defs ts

> typesDeps :: VariableScopes -> [GramType] -> [GramId]
> typesDeps defs ts = concat $ map (concat . map (varDependency defs) . nub . freeTypeVars) ts

> freeTypeVars :: GramType -> [GramId]
> freeTypeVars (GramIdType vid@(Id _ id)) 
>   | isPrefixOf "_v" id = [tfName vid]
>   | otherwise = []
> freeTypeVars (GramListType _ t) = freeTypeVars t
> freeTypeVars (GramTupleType _ t1 t2) = freeTypeVars t1 ++ freeTypeVars t2
> freeTypeVars (GramForAllType _ bound tinner) = (nub $ freeTypeVars tinner) \\ (map tfName bound)
> freeTypeVars (GramFunType _ (GramFunTypeAnnot targs tret)) = 
>   let tvars = concat $ map freeTypeVars targs in 
>     case tret of
>       GramVoidType _ -> tvars
>       GramRetType  t -> tvars ++ freeTypeVars t
> freeTypeVars _ = []

> tfName :: GramId -> GramId
> tfName (Id _ id) = Id (newPos "<internal>" 0 0) $ "__tf_" ++ drop 2 id

> getGlobals :: [GramDecl] -> [GramId]
> getGlobals decls = builtIns ++ map getName decls
>   where getName (GramDeclFun (GramFuncDecl fid _ _ _))  = fid
>         getName (GramDeclVar (GramVarDeclType _ vid _)) = vid
>         getName (GramDeclVar (GramVarDeclVar    vid _)) = vid

> builtIns :: [GramId]
> builtIns = map bi ["print", "println", "display", "isEmpty", "chr", "ord", "error"]
>   where bi = Id $ newPos "<builtin>" 0 0


Scope handlers

> newBlock :: VariableScopes -> VariableScopes
> newBlock (funscope:defs) = ([]:funscope):defs

> newFunc :: VariableScopes -> [GramId] -> VariableScopes
> newFunc defs ids = [[],ids]:defs

> addToScope :: VariableScopes -> [GramId] -> VariableScopes
> addToScope ((blocklocals:funlocals):defs) ids = ((ids ++ blocklocals):funlocals):defs

> globalScope :: VariableScopes -> [GramId]
> globalScope defs = last $ last defs

> varDependency :: VariableScopes -> GramId -> [GramId]
> varDependency defs id = varDependency' True defs id
>   where matchId (Id _ qid) (Id _ vid) = qid == vid
>         varInFunScope [] _ = Nothing
>         varInFunScope (blockscope:blockscopes) id = 
>           case find (matchId id) blockscope of
>             Just vid -> Just vid
>             Nothing  -> varInFunScope blockscopes id
>         varDependency' _ [] id@(Id _ qid)
>           | isPrefixOf "__tf_" qid = [id] 
>           | otherwise = []
>         varDependency' top (funscope:funscopes) id =
>           case varInFunScope funscope id of
>             Just vid -> if top then [] else [vid]
>             Nothing  -> varDependency' False funscopes id
