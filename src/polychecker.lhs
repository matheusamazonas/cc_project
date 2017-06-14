> module PolyChecker where

> import Control.Monad
> import Control.Monad.Except
> import Control.Monad.State
> import Data.Ord (comparing)
> import Data.List ((\\), find, intersect, isPrefixOf, nub, sortBy)
> import Dependency
> import Grammar
> import Text.Parsec.Pos (newPos, SourcePos)
> import Text.Read (readMaybe)
> import Token 
> import Error

> data Type = TBool | TInt | TChar | TVoid | TTuple Type Type | TList Type | TFunc [Type] Type | TVar VId | TBound VId | TSkolem VId | TForAll [VId] Type
>   deriving (Show, Eq, Ord)

> type Monotype = Type -- no foralls
> type RhoType = Type -- weak-prenex form: no top-level foralls (i.e., monotype or [polytype] -> polytype)
> type Polytype = Type -- either monotype or forall a. rhotype for a number of a's
> type VId = Int

> data Expected t = Infer t | Check t

> data Returns = DoesNotReturn | SometimesReturns | AlwaysReturns
>   deriving (Show, Eq)

> type SubList = [(VId, Type)]
> type Scope = [(String, VId)]
> type EnvType = (SubList, [Scope], VId)

> type Environment = StateT EnvType (Either CompilationError)


===============================================================================
===============================================================================
PolyChecker

Performs type checking and inference, and syntax-tree annotation.

This consists of the following major stages:
1. dependency analysis
2. block-wise type inference and initial tree decoration
3. post-inference decoration

1. Dependency analysis
See dependency.lhs. This provides definition order independence and 
divides the tree into mutually recursive blocks.
It is part of the type checker, but it is called in main.lhs 
so that the type checker does not need to needlessly propagate captures.

2. Block-wise type inference and initial tree decoration
Applied per mutually recursive block, and divided into the following stages:

2a. Initial variable/function declarations
    Names are registered in the scope and type variables are set up
	for variables, function arguments and return values,
	so that blocks' type inference is order-independent.
2b. Type inference and initial tree decoration
    The entire tree is traversed and type inference/checking is performed
	based on the bidirectional Odersky-Läufer typing rules,
	as described in [1]. Variable assignments and other syntactic structures
	that were not in [1] were rewritten in terms of typing judgments within 
	this system, and additional machinery for e.g. by-reference variables and
	overloading was added on top of this where needed.
	Tree decoration is performed on function calls and overloaded operators 
	during traversal, meaning type variables are left in annotations 
	when still unresolved (see step 3).
2c. Generalisation and global type annotations
    Functions with unresolved argument types are generalised
	to polymorphic type schemes, such as in e.g. the identity function.
	This can only happen after the entire mutually recursive block has been
	processed by type inference, hence the need for a separate stage. 
	Furthermore, global variables and functions are annotated with their types.

3. Post-inference decoration
Traverses the tree once more, to finalise tree decorations by substituting in
all known type resolutions. This is necessary for order-independence, as
stage 2 annotates variables, expressions and function calls sequentially, 
possibly before type information becomes available.

Notes:
Called with inferProg, returning only the decorated tree.
[1] Jones, S. P., Vytiniotis, D., Weirich, S., & Shields, M. (2007).
    Practical type inference for arbitrary-rank types.
	Journal of functional programming, 17(01), 1-82.
===============================================================================
===============================================================================


===============================================================================
 Known issues / improvements
===============================================================================

function pr1(x) {
  function doit() {
    print(x);
  }
  return doit;
}

function main() {
  var f = pr1(1);
  f();
}

f() function call not annotated; annotate function calls with their capture
(which needs to be propagated throughout polychecker)
test more complicated ones than this


===============================================================================
 Top-level structure
===============================================================================

> inferProg :: [[GramDecl]] -> Either CompilationError [GramDecl]
> inferProg declBlocks =
>   case runStateT (inference declBlocks) initEnv of
>     Left e             -> Left e
>     Right (annot, env) -> Right annot
>   where inference :: [[GramDecl]] -> Environment [GramDecl]
>         inference declBlocks = do
>           decls <- inferDeclBlocks declBlocks
>           postDecorate decls
>         inferDeclBlocks :: [[GramDecl]] -> Environment [GramDecl]
>         inferDeclBlocks [] = return []
>         inferDeclBlocks (block:blocks) = do
>           newblock <- inferDeclBlock block
>           blocklist <- inferDeclBlocks blocks
>           return $ newblock ++ blocklist

> inferDeclBlock :: [GramDecl] -> Environment [GramDecl]
> inferDeclBlock ds = do
>   checkRecursiveVars ds
>   vs <- inferDeclsHeader ds
>   ds <- inferDeclsBody ds
>   if any isFunDecl ds then generalise vs
>   else return ()
>   inferDeclsPost ds
>   where inferDeclsHeader [] = return []
>         inferDeclsHeader (decl:decls) = do
>           v <- case decl of
>             GramDeclVar vardecl -> inferVarDeclHeader vardecl
>             GramDeclFun fundecl -> inferFunDeclHeader fundecl 
>           vs <- inferDeclsHeader decls
>           return $ v:vs
>         inferDeclsBody [] = return []
>         inferDeclsBody (decl:decls) = do
>           decl <- case decl of
>             GramDeclVar vardecl -> do
>               vardecl <- inferVarDeclBody vardecl
>               return $ GramDeclVar vardecl
>             GramDeclFun fundecl -> do
>               fundecl <- inferFunDeclBody fundecl 
>               return $ GramDeclFun fundecl
>           decls <- inferDeclsBody decls
>           return $ decl:decls
>         inferDeclsPost [] = return []
>         inferDeclsPost (decl:decls) = do
>           decl <- case decl of
>             GramDeclVar vardecl -> do
>               vardecl <- inferVarDeclPost vardecl
>               return $ GramDeclVar vardecl
>             GramDeclFun fundecl -> do
>               fundecl <- inferFunDeclPost fundecl
>               return $ GramDeclFun fundecl
>           decls <- inferDeclsPost decls
>           return $ decl:decls
>         isFunDecl (GramDeclFun _) = True
>         isFunDecl _ = False

> checkRecursiveVars :: [GramDecl] -> Environment () -- forbids otherwise allowed "var flip = 0:flop; var flop = 1:flip;"
> checkRecursiveVars decls = case find isVar decls of
>   Nothing   -> return ()
>   Just decl -> 
>     case () of 
>       _ | length decls > 1 ->
>             let Id p i = getId decl in
>               throwError $ CompilationError TypeChecker ("Mutually recursive variable definitions are not allowed: " ++ i) p
>         | otherwise -> return ()
>   where isVar (GramDeclVar _) = True
>         isVar _ = False
>         getId (GramDeclVar (GramVarDeclType _ id _)) = id
>         getId (GramDeclVar (GramVarDeclVar id _)) = id

===============================================================================
 Global declarations - stage 2a
===============================================================================

> inferVarDeclHeader :: GramVarDecl -> Environment Type
> inferVarDeclHeader vardecl = declareVar $ getName vardecl
>   where getName (GramVarDeclType _ vid _) = vid
>         getName (GramVarDeclVar    vid _) = vid

> inferFunDeclHeader :: GramFuncDecl -> Environment Type
> inferFunDeclHeader (GramFuncDecl id@(Id p i) fargs [] stmts) = do -- let func = \arg1. (...) \argn. stmts
>   vfun <- declareVar id
>   vargs <- mapM (\_ -> fresh) fargs
>   vret <- fresh
>   let tfun = TFunc vargs vret
>   unify p vfun tfun
>   return vfun
> inferFunDeclHeader (GramFuncDecl id@(Id p i) fargs [GramFunTypeAnnot ftypes tret] stmts) = do -- let func = (\arg1. (...) \argn. stmts) :: t1 (...) tn -> tret
>   vfun <- declareVar id
>   vargs <- mapM (\_ -> fresh) fargs
>   vret <- fresh
>   (arglist, insts) <- listArgTypes id [] vargs ftypes
>   tret <- retType p insts vret tret
>   let tfun = TFunc arglist tret
>   unify p vfun tfun
>   return vfun
>   where listArgTypes _ insts [] [] = return ([], insts)
>         listArgTypes id@(Id pos _) insts (v:vars) (t:ftypes) = do
>           (newinsts,v) <- unifyWith pos insts v t
>           (arglist, finsts) <- listArgTypes id newinsts vars ftypes
>           return $ (v:arglist, finsts)
>         listArgTypes id@(Id pos i) _ _ _ = throwError $ CompilationError TypeChecker ("Mismatching number of arguments in type signature: " ++ i) pos
>         retType pos insts v (GramRetType t)  = do
>           (_,tret) <- unifyWith pos insts v t
>           return tret
>         retType _ insts v (GramVoidType p) = do
>           unify p v TVoid
>           return TVoid

===============================================================================
 High-level type inference - stage 2b
===============================================================================

> inferVarDeclBody :: GramVarDecl -> Environment GramVarDecl
> inferVarDeclBody vardecl = case vardecl of
>   GramVarDeclType annot varId expr -> do -- let var = (exp::t)
>     (i, p, v, e, vid) <- inferVarDeclTailPre varId expr
>     (_,poly) <- unifyWith p [] v annot
>     e <- addErrorDesc ("Variable type annotation does not match given expression (" ++ i ++ "): ") $ replErr i $ checkTypePoly e poly
>     generalise [v]
>     inferVarDeclTailPost varId expr v vid
>     return $ GramVarDeclType annot (Id p i) e
>   GramVarDeclVar varId expr    -> do -- let var = exp
>     (i, p, v, e, vid) <- inferVarDeclTailPre varId expr
>     e <- addErrorDesc ("Variable declaration failed (" ++ i ++ "): ") $ replErr i $ inferExpr e $ Infer v
>     generalise [v]
>     inferVarDeclTailPost varId expr v vid
>     return $ GramVarDeclVar (Id p i) e
>   where inferVarDeclTailPre id@(Id p i) e = do
>           v <- getVarType id
>           vid <- removeFromScope id
>           return (i, p, v, e, vid)
>         inferVarDeclTailPost id@(Id p i) e v vid = do
>           v <- convert v
>           addToScope id vid
>           if occurs vid v then throwError $ CompilationError TypeChecker ("Recursive variable definition detected: " ++ i) p
>           else return ()
>         replErr var = replaceErrorType ("Variable out of scope: " ++ var) ("Recursive variable definition detected: " ++ var)

> inferFunDeclBody :: GramFuncDecl -> Environment GramFuncDecl
> inferFunDeclBody (GramFuncDecl id@(Id p i) fargs ftypes stmts) = do 
>   fid <- getVarId id
>   tfun <- getVarType id
>   let TFunc arglist vret = tfun
>   pushScope
>   declareArgs fargs (fid+1)
>   pushScope
>   let texp = expected ftypes vret
>   (tret,stmts) <- inferStmtBlock p stmts texp
>   popScope
>   popScope
>   case tret of 
>     DoesNotReturn -> assertType p TVoid texp
>     SometimesReturns -> do
>       vret <- convert vret
>       if vret == TVoid then return ()
>       else throwError $ CompilationError TypeChecker ("Non-void function contains non-returning code paths: " ++ i) p
>     AlwaysReturns -> return ()
>   return $ GramFuncDecl id fargs ftypes stmts
>   where declareArgs [] _ = return ()
>         declareArgs (aid:fargs) ai = do
>           addToScope aid ai
>           declareArgs fargs (ai+1)
>         expected [] t = Infer t
>         expected _  t = Check t

===============================================================================
 Global-level annotations - stage 2c
===============================================================================

> inferVarDeclPost :: GramVarDecl -> Environment GramVarDecl
> inferVarDeclPost (GramVarDeclVar vid expr) = do
>   t <- getVarType vid
>   return $ GramVarDeclType (convertToGramType t) vid expr
> inferVarDeclPost (GramVarDeclType _ vid expr) = do
>   t <- getVarType vid
>   return $ GramVarDeclType (convertToGramType t) vid expr

> inferFunDeclPost :: GramFuncDecl -> Environment GramFuncDecl
> inferFunDeclPost (GramFuncDecl id fargs _ stmts) = do
>   tfun <- getVarType id
>   let ftypes = [funType tfun]
>   return $ GramFuncDecl id fargs ftypes stmts
>   where funType (TFunc targs tret) = GramFunTypeAnnot (map convertToGramType targs) (retType tret)
>         funType (TForAll _ t) = funType t
>         retType TVoid = GramVoidType nP
>         retType t = GramRetType $ convertToGramType t



===============================================================================
 Function type inference/checking - stage 2b
===============================================================================

Statements, unlike expressions, do not always have a return value.
Thus, we cannot use regular type judgments on their return types to determine
a block's return type. Furthermore, we need to make sure that functions always
return something (or void), and so we need to establish whether all branches
return, and finally that they also return values of the same type.
To do all of these, we keep track of whether blocks never, sometimes
or always return, and assert that all values returned must be 
of equivalent type. For monotypes, this is regular unification, but
(higher-order) polymorphic function signatures that are not identical 
may be equivalent. We thus assert that neither polymorphic return type
can be more polymorphic than the other, with respect to deep skolemisation.
A description of (mutual) deep skolemisation can be found in [1].

> inferStmtBlock :: SourcePos -> [GramStmt] -> Expected Polytype -> Environment (Returns, [GramStmt])
> inferStmtBlock p [] texp = do
>   return (DoesNotReturn, [])
> inferStmtBlock _ (stmt:stmts) texp = do 
>   (p, tret1, stmt) <- inferStmt stmt texp
>   case tret1 of 
>     DoesNotReturn    -> do
>       (tret2, stmts) <- inferStmtBlock p stmts texp
>       return (tret2, stmt:stmts)
>     AlwaysReturns    ->
>       if null stmts then return $ (AlwaysReturns, [stmt])
>       else throwError $ CompilationError TypeChecker ("Unreachable code detected") p
>     SometimesReturns -> do
>       texp2 <- expectedSubtype texp
>       (tret2, stmts) <- inferStmtBlock p stmts texp2
>       case tret2 of
>         DoesNotReturn -> return (SometimesReturns, stmt:stmts)
>         AlwaysReturns -> do
>           addErrorDesc "Inconsistent return types: " $ equivalent p texp texp2 
>           return (AlwaysReturns, stmt:stmts)
>         SometimesReturns -> do
>           addErrorDesc "Inconsistent return types: " $ equivalent p texp texp2 
>           return (SometimesReturns, stmt:stmts)

> inferStmt :: GramStmt -> Expected RhoType -> Environment (SourcePos, Returns, GramStmt)
> inferStmt (GramIf p cond tr fa) texp = do -- if statement as described in sec. 7.1 [1]
>   cond <- addErrorDesc "Non-boolean expression used as branch condition: " $ inferExpr cond $ Check TBool
>   exptr <- expectedSubtype texp
>   expfa <- expectedSubtype texp
>   pushScope
>   (ret1, tr) <- inferStmtBlock p tr exptr
>   popScope 
>   pushScope
>   (ret2, fa) <- inferStmtBlock p fa expfa
>   popScope
>   let stmt = GramIf p cond tr fa
>   if ret1 == DoesNotReturn then (
>     if ret2 == DoesNotReturn then return (p, DoesNotReturn, stmt)
>     else do
>       propagateSubtype expfa texp
>       return (p, SometimesReturns, stmt) )
>   else if ret2 == DoesNotReturn then do
>       propagateSubtype exptr texp
>       return (p, SometimesReturns, stmt)
>     else do
>       addErrorDesc "Inconsistent return types: " $ equivalent p exptr expfa 
>       propagateSubtype exptr texp
>       if (ret1 == AlwaysReturns && ret2 == AlwaysReturns) then return (p, AlwaysReturns, stmt)
>       else return (p, SometimesReturns, stmt)
> inferStmt (GramWhile p cond lp) texp = do -- typed equivalently to if(cond) { lp } else { }
>   cond <- addErrorDesc "Non-boolean expression used as loop condition: " $ inferExpr cond $ Check TBool
>   explp <- expectedSubtype texp
>   pushScope
>   (tret, lp) <- inferStmtBlock p lp explp
>   popScope
>   let stmt = GramWhile p cond lp
>   if tret == DoesNotReturn then return (p, DoesNotReturn, stmt)
>   else do
>     propagateSubtype explp texp
>     return (p, SometimesReturns, stmt)
> inferStmt (GramReturn p ret) texp = -- regular typing judgment
>   case ret of
>     Nothing -> do
>       assertType p TVoid texp
>       return $ (p, AlwaysReturns, GramReturn p Nothing)
>     Just e  -> do
>       e <- inferExpr e texp
>       return $ (p, AlwaysReturns, GramReturn p $ Just e)
> inferStmt (GramFunVarDecl vardecl) texp = do -- let var = exp, same as global variable declaration
>   inferVarDeclHeader vardecl
>   vardecl <- inferVarDeclBody vardecl
>   vardecl <- inferVarDeclPost vardecl
>   return (getPos vardecl, DoesNotReturn, GramFunVarDecl vardecl)
>   where getPos (GramVarDeclType _ (Id p i) _) = p
>         getPos (GramVarDeclVar    (Id p i) _) = p
> inferStmt (GramAttr p (Var id@(Id pos i) fields) e) texp = do -- let var = (exp::t) (global typed variable declaration), but with fields
>   vid <- getVarId id
>   t <- getVarType id
>   if vid < 0 then throwError $ CompilationError TypeChecker ("Cannot override built-in function " ++ i) pos
>   else do
>     tfield <- traverseFields fields p t
>     e <- checkTypePoly e tfield
>     generalise [tfield]
>     return (p, DoesNotReturn, GramAttr p (Var id fields) e)
> inferStmt (GramStmtFuncDecl fundecl) texp = do -- let func = funcbody, same as global function declaration
>   vfun <- inferFunDeclHeader fundecl
>   fundecl <- inferFunDeclBody fundecl
>   generalise [vfun]
>   fundecl <- inferFunDeclPost fundecl
>   return (getPos fundecl, DoesNotReturn, GramStmtFuncDecl fundecl)
>   where getPos (GramFuncDecl (Id p _) _ _ _) = p
> inferStmt (GramStmtFunCall funcall) texp = do -- return value is irrelevant, just type-check argument types
>   v <- fresh
>   (p,_,_,funcall) <- inferFunCall funcall $ Infer v
>   return (p, DoesNotReturn, GramStmtFunCall funcall)

===============================================================================
 Type inference/checking algorithm M (Damas-Hindley-Milner) - stage 2b
===============================================================================

> inferExpr :: GramExp -> Expected RhoType -> Environment GramExp
> inferExpr orig@(GramBool p _) t = do
>   assertType p TBool t
>   return orig
> inferExpr orig@(GramChar p _) t = do 
>   assertType p TChar t
>   return orig
> inferExpr orig@(GramNum p _)  t = do
>   assertType p TInt t
>   return orig
> inferExpr orig@(GramEmptyList p) t = do
>   v <- fresh
>   assertType p (TList v) t
>   return orig
> inferExpr (GramExpTuple p e1 e2) t = do -- just propagate the type-check/inference through the tuple
>   v1 <- fresh
>   v2 <- fresh
>   assertType p (TTuple v1 v2) t
>   v1 <- convert v1
>   e1 <- inferExpr e1 $ v1 `sameDirAs` t
>   v2 <- convert v2
>   e2 <- inferExpr e2 $ v2 `sameDirAs` t
>   return $ GramExpTuple p e1 e2
>   where sameDirAs v (Check _) = Check v
>         sameDirAs v (Infer _) = Infer v
> inferExpr (GramOverloadedBinary p _ op e1 e2) t = inferExpr (GramBinary p op e1 e2) t
> inferExpr (GramBinary p op e1 e2) t = do -- operators are seen as functions in the typing system
>   (t1, t2, tret, ol) <- opType op
>   e1 <- inferExpr e1 $ Check t1 -- for overloaded operations: type-checking w.r.t. a TVar
>   t2 <- convert t2              -- reduces to regular unification, which is what we want,
>   e2 <- inferExpr e2 $ Check t2 -- rather than Infer which instantiates the inferred type
>   tret <- convert tret
>   assertType p tret t
>   t1 <- convert t1
>   if ol then return $ GramOverloadedBinary p (convertToGramType t1) op e1 e2
>   else return $ GramBinary p op e1 e2
> inferExpr (GramUnary p op e) t = do -- operators are seen as functions in the typing system
>   (tel, _, tret, _) <- opType op
>   e <- inferExpr e $ Check tel
>   assertType p tret t
>   return $ GramUnary p op e
> inferExpr orig@(GramExpId (Var vid@(Id p i) fields)) t = do
>   vart <- getVarType vid
>   tfield <- traverseFields fields p vart
>   assertType p tfield t
>   return orig
> inferExpr (GramExpFunCall funcall) t = do
>   (p,i,tret,funcall) <- inferFunCall funcall t
>   if tret == TVoid then throwError $ CompilationError TypeChecker ("Void function (" ++ i ++ ") used in an expression") p
>   else return $ GramExpFunCall funcall

> inferFunCall :: GramFunCall -> Expected RhoType -> Environment (SourcePos, String, Type, GramFunCall)
> inferFunCall (GramOverloadedFunCall _ id args) texp = inferFunCall (GramFunCall id args) texp
> inferFunCall orig@(GramFunCall id@(Id p i) args) texp = do -- rule APP
>   tpolyfun <- getVarType id
>   tfun <- instantiate tpolyfun
>   (argtypes, tret) <- assertFunc p (length args) tfun
>   (args, argtypes) <- inferArgs id args argtypes
>   tret <- convert tret
>   assertType p tret texp
>   tret <- convert tret
>   funid <- getVarId id
>   let vfun = convertToGramType $ TVar funid
>   return (p, i, tret, GramOverloadedFunCall (vfun:argtypes) id args) -- this way, all argument types are included,
>   where inferArgs _ [] [] = return ([],[])                           -- so that in post-decoration we can decide which ones
>         inferArgs (Id _ i) (e:es) (targ:targs) = do                  -- to include as actual overloaded type annotations
>           e <- addErrorDesc ("Argument to function " ++ i ++ " has wrong type: ") $ checkTypePoly e targ
>           (es,targs) <- inferArgs id es targs
>           targ <- convert targ
>           return (e:es, convertToGramType targ : targs)
>         inferArgs (Id pos i) _ _ = throwError $ CompilationError TypeChecker ("Mismatching number of arguments given to function: " ++ i) pos
>         isPolymorph (TForAll _ _) = True
>         isPolymorph _ = False

> traverseFields :: [GramField] -> SourcePos -> Type -> Environment Type
> traverseFields [] _ vart = return vart
> traverseFields [First p fields] _ vart = do
>   v1 <- fresh
>   v2 <- fresh
>   addErrorDesc "fst applied to non-tuple variable: " $ unify p (TTuple v1 v2) vart
>   v1 <- convert v1
>   traverseFields fields p v1 
> traverseFields [Second p fields] _ vart = do
>   v1 <- fresh
>   v2 <- fresh
>   addErrorDesc "snd applied to non-tuple variable: " $ unify p (TTuple v1 v2) vart
>   v2 <- convert v2
>   traverseFields fields p v2
> traverseFields [Head p fields] _ vart = do
>   v <- fresh
>   addErrorDesc "hd applied to non-list variable: " $ unify p (TList v) vart
>   v <- convert v
>   traverseFields fields p v
> traverseFields [Tail p fields] _ vart = do
>   v <- fresh
>   addErrorDesc "tl applied to non-list variable: " $ unify p (TList v) vart
>   v <- convert v
>   traverseFields fields p (TList v)

> opType :: Operation -> Environment (Type, Type, Type, Bool)
> opType op
>   | op `elem` [Minus, Plus, Times, Division, Mod] = return (TInt, TInt, TInt, False)
>   | op `elem` [LessThan, LessOrEqual, GreaterThan, GreaterOrEqual] = return (TInt, TInt, TBool, False)
>   | op `elem` [LogicalOr, LogicalAnd, LogicalNot] = return (TBool, TBool, TBool, False)
>   | op `elem` [Equals, Different] = do { v <- fresh; return (v, v, TBool, True) }
>   | op == ListConst = do { v <- fresh; return (v, TList v, TList v, False) }



===============================================================================
 Post-decoration - stage 3
===============================================================================

> postDecorate :: [GramDecl] -> Environment [GramDecl]
> postDecorate [] = return []
> postDecorate (decl:decls) = do
>   decl  <- postDecorateDecl decl
>   decls <- postDecorate decls
>   return $ decl:decls

> postDecorateDecl :: GramDecl -> Environment GramDecl
> postDecorateDecl (GramDeclVar vardecl) = do
>   vardecl <- postDecorateVarDecl vardecl
>   return $ GramDeclVar vardecl
> postDecorateDecl (GramDeclFun fundecl) = do
>   fundecl <- postDecorateFunDecl fundecl
>   return $ GramDeclFun fundecl 

> postDecorateVarDecl :: GramVarDecl -> Environment GramVarDecl
> postDecorateVarDecl (GramVarDeclType t id e) = do
>   t <- convertGramType t
>   e <- postDecorateExpr e
>   return $ GramVarDeclType t id e

> postDecorateFunDecl :: GramFuncDecl -> Environment GramFuncDecl
> postDecorateFunDecl (GramFuncDecl id fargs [GramFunTypeAnnot ftypes tret] stmts) = do
>   stmts <- postDecorateBlock stmts
>   ftypes <- convertFTypes ftypes
>   tret <- convertRetType tret
>   let funtype = GramFunTypeAnnot ftypes tret
>   return $ GramFuncDecl id fargs [funtype] stmts
>   where convertFTypes [] = return []
>         convertFTypes (t:ftypes) = do
>           t <- convertGramType t
>           ftypes <- convertFTypes ftypes
>           return (t:ftypes)
>         convertRetType (GramRetType t) = do
>           t <- convertGramType t
>           return $ GramRetType t
>         convertRetType (GramVoidType p) = return $ GramVoidType p

> postDecorateBlock :: [GramStmt] -> Environment [GramStmt]
> postDecorateBlock [] = return []
> postDecorateBlock (stmt:stmts) = do
>   stmt <- postDecorateStmt stmt
>   stmts <- postDecorateBlock stmts
>   return $ stmt:stmts

> postDecorateStmt :: GramStmt -> Environment GramStmt
> postDecorateStmt (GramIf p e tr fa) = do
>   e <- postDecorateExpr e
>   tr <- postDecorateBlock tr
>   fa <- postDecorateBlock fa
>   return $ GramIf p e tr fa
> postDecorateStmt (GramWhile p e lp) = do
>   e <- postDecorateExpr e
>   lp <- postDecorateBlock lp
>   return $ GramWhile p e lp
> postDecorateStmt (GramReturn p ret) = do
>   ret <- case ret of
>     Nothing -> return Nothing
>     Just e  -> do
>       e <- postDecorateExpr e
>       return $ Just e
>   return $ GramReturn p ret
> postDecorateStmt (GramFunVarDecl vardecl) = do
>   vardecl <- postDecorateVarDecl vardecl
>   return $ GramFunVarDecl vardecl
> postDecorateStmt (GramAttr p v e) = do
>   e <- postDecorateExpr e
>   return $ GramAttr p v e
> postDecorateStmt (GramStmtFuncDecl fundecl) = do
>   fundecl <- postDecorateFunDecl fundecl
>   return $ GramStmtFuncDecl fundecl
> postDecorateStmt (GramStmtFunCall funcall) = do
>   (_,_,_,funcall) <- postDecorateFunCall funcall
>   return $ GramStmtFunCall funcall

> postDecorateExpr :: GramExp -> Environment GramExp
> postDecorateExpr (GramExpTuple p e1 e2) = do
>   e1 <- postDecorateExpr e1
>   e2 <- postDecorateExpr e2
>   return $ GramExpTuple p e1 e2
> postDecorateExpr (GramBinary p op e1 e2) = do
>   e1 <- postDecorateExpr e1
>   e2 <- postDecorateExpr e2
>   return $ GramBinary p op e1 e2
> postDecorateExpr (GramOverloadedBinary p t op e1 e2) = do
>   e1 <- postDecorateExpr e1
>   e2 <- postDecorateExpr e2
>   t <- convertGramType t
>   if typeAllowed t then return $ GramOverloadedBinary p t op e1 e2
>   else throwError $ CompilationError TypeChecker ("Equality is not defined for functions") p
>   where typeAllowed (GramForAllType _ _ _) = False
>         typeAllowed (GramFunType _ _) = False
>         typeAllowed _ = True
> postDecorateExpr (GramUnary p op e) = do
>   e <- postDecorateExpr e
>   return $ GramUnary p op e
> postDecorateExpr (GramExpFunCall funcall) = do
>   (tret,i,p,funcall) <- postDecorateFunCall funcall
>   if tret == TVoid then throwError $ CompilationError TypeChecker ("Void function used in an expression: " ++ i) p -- catches otherwise uncaught "f() { return g(); } g() { f(); }"
>   else return $ GramExpFunCall funcall
> postDecorateExpr e = return e

> postDecorateFunCall :: GramFunCall -> Environment (Type, String, SourcePos, GramFunCall)
> postDecorateFunCall (GramOverloadedFunCall ts id@(Id p i) es) = do -- all function calls are internally "overloaded" until post-decoration, see inferFuncall
>   ts <- mapM convertGramType ts
>   es <- mapM postDecorateExpr es
>   tfun <- convertFromGramType $ head ts
>   let (targs, tret) = splitFunc tfun
>   tret <- convert tret
>   targs <- mapM convert targs
>   let annots = evalState (typeAnnotations (map convertToGramType targs) $ tail ts) []
>   if null annots then return (tret, i, p, GramFunCall id es)
>   else return (tret, i, p, GramOverloadedFunCall annots id es)
>   where typeAnnotations :: [GramType] -> [GramType] -> State [String] [GramType]
>         typeAnnotations [] [] = return []
>         typeAnnotations (tsig:tsigs) (targ:targs) = do
>           annot <- typeAnnotation tsig targ
>           annots <- typeAnnotations tsigs targs
>           return $ annot ++ annots
>         typeAnnotation :: GramType -> GramType -> State [String] [GramType]
>         typeAnnotation (GramIdType (Id _ id)) t
>           | isPrefixOf "_v" id = do
>             ids <- get
>             if drop 2 id `elem` ids then return []
>             else do
>               put $ (drop 2 id):ids
>               return [t]
>           | otherwise = return []
>         typeAnnotation (GramListType _ tsig) (GramListType _ targ) = typeAnnotation tsig targ
>         typeAnnotation (GramTupleType _ tsig1 tsig2) (GramTupleType _ targ1 targ2) = do
>           annots1 <- typeAnnotation tsig1 targ1 
>           annots2 <- typeAnnotation tsig2 targ2
>           return $ annots1 ++ annots2
>         typeAnnotation (GramFunType _ (GramFunTypeAnnot sigtypes sigret)) (GramFunType _ (GramFunTypeAnnot argtypes argret)) = do
>           let t1s = case sigret of (GramVoidType _) -> sigtypes    -- note that there can be no
>                                    (GramRetType t1) -> t1:sigtypes -- discrepancy between the two
>           let t2s = case argret of (GramVoidType _) -> argtypes    -- types in e.g. voidness/non-
>                                    (GramRetType t2) -> t2:argtypes -- voidness; this is guaranteed by
>           typeAnnotations t1s t2s                                  -- the type checking system
>         typeAnnotation (GramForAllType _ bound tinner) t = do
>           ids <- get
>           put $ ids ++ map (\(Id _ i) -> drop 2 i) bound
>           typeAnnotation tinner t
>         typeAnnotation t (GramForAllType _ bound tinner) = do
>           ids <- get
>           put $ ids ++ map (\(Id _ i) -> drop 2 i) bound
>           typeAnnotation t tinner
>         typeAnnotation _ _ = return []
>         splitFunc (TFunc targs tret) = (targs, tret)
>         splitFunc (TForAll _ t) = splitFunc t



===============================================================================
 Unification algorithm U - stage 2b
===============================================================================

> unify :: SourcePos -> Monotype -> Monotype -> Environment ()
> unify p t1 t2 = do 
>   t1 <- convert t1
>   t2 <- convert t2
>   sub <- lift $ unification p t1 t2
>   apply sub

> unification :: SourcePos -> Monotype -> Monotype -> Either CompilationError SubList
> unification _ TVoid TVoid  = Right []
> unification _ TChar TChar  = Right []
> unification _ TInt TInt    = Right []
> unification _ TBool TBool  = Right []
> unification p (TVar i) t
>   | t == (TVar i)      = Right []
>   | occurs i t             = Left $ CompilationError TypeChecker "Recursive type detected" p
>   | otherwise              = Right [(i, t)]
> unification p t (TVar i)
>   | t == (TVar i)      = Right []
>   | occurs i t             = Left $ CompilationError TypeChecker "Recursive type detected" p
>   | otherwise              = Right [(i, t)]
> unification p (TSkolem i) t
>   | t == TSkolem i         = Right []
>   | otherwise              = Left $ CompilationError TypeChecker "Polymorphic type was less general than was required (subsumption check failed)" p
> unification p t (TSkolem i)
>   | t == TSkolem i         = Right []
>   | otherwise              = Left $ CompilationError TypeChecker "Polymorphic type was less general than was required (subsumption check failed)" p
> unification p (TList ta) (TList tb) = unification p ta tb
> unification p (TTuple ta1 ta2) (TTuple tb1 tb2) = do
>   sub1 <- unification p ta1 tb1
>   sub2 <- unification p (sub1 |-> ta2) (sub1 |-> tb2)
>   return $ sub1 ++ sub2
> unification p (TFunc [] ta2) (TFunc [] tb2) = unification p ta2 tb2
> unification p (TFunc (ta1:ta1s) ta2) (TFunc (tb1:tb1s) tb2) = do
>   sub1 <- unification p ta1 tb1
>   sub2 <- unification p (sub1 |-> (TFunc ta1s ta2)) (sub1 |-> (TFunc tb1s tb2))
>   return $ sub1 ++ sub2
> unification p t1 t2 = Left $ CompilationError TypeChecker ("Can't unify types " ++ show t1 ++ " and " ++ show t2) p

> occurs :: VId -> Monotype -> Bool
> occurs i (TTuple ta tb)          = (occurs i ta) || (occurs i tb) -- implements occurs check
> occurs i (TList t)               = occurs i t
> occurs i (TFunc [] tb)           = (occurs i tb)
> occurs i (TFunc (ta1:ta1s) tb)   = (occurs i ta1) || (occurs i (TFunc ta1s tb))
> occurs i (TVar j)                = i == j
> occurs _ _ = False

> (|->) :: SubList -> Type -> Type
> (|->) sub (TTuple ta tb)      = TTuple (sub |-> ta) (sub |-> tb)
> (|->) sub (TList t)           = TList (sub |-> t)
> (|->) sub (TFunc tas tb)      = TFunc (map (sub |->) tas) (sub |-> tb)
> (|->) sub (TVar i)            = case lookup i sub of Just subbed -> sub |-> subbed
>                                                      Nothing     -> (TVar i)
> (|->) sub (TBound i)          = case lookup i sub of Just subbed -> sub |-> subbed -- used during instantiation, skolemisation
>                                                      Nothing     -> (TBound i)
> (|->) sub (TForAll vids t)    = TForAll vids (sub |-> t)
> (|->) _ t                     = t
> infixr 3 |->



===============================================================================
 Higher-rank type inference/checking - stage 2b/c
===============================================================================

> generalise :: [Type] -> Environment () -- env |-poly_up t : sigma
> generalise vars = do -- rule GEN1
>   let vids = map vid vars
>   vartypes <- mapM convert vars
>   tenvs <- envTypes
>   tenvvars <- typeVars $ tenvs \\ vartypes
>   tvars <- typeVars vartypes
>   let internals = tvars \\ tenvvars
>   mapM_ (generalise' internals) $ zip vids vartypes
>   where vid (TVar i) = i
>         generalise' blockinternals (vid,tvar) = do
>           varinternals <- typeVars [tvar]
>           quantify vid (varinternals `intersect` blockinternals) tvar

> checkTypePoly :: GramExp -> Polytype -> Environment GramExp -- env |-poly_down t : sigma
> checkTypePoly e t = do -- rule GEN2
>   t <- convert t
>   (skolems, wpt) <- skolemise t
>   e <- inferExpr e $ Check wpt
>   tenvs <- envTypes
>   tfree <- freeVars $ t:tenvs
>   if any (`elem` tfree) $ map snd skolems then throwError $ CompilationError TypeChecker ("The given expression was not polymorphic enough") (getExpPos e)
>   else return e



===============================================================================
 Polytype subsumption - stage 2b/c
===============================================================================

> subsumesPoly :: SourcePos -> Polytype -> Polytype -> Environment () -- |-dsk sigma <= sigma'
> subsumesPoly p poly1 poly2 = do -- rule DEEP-SKOL
>   (skolemSubs, wp2) <- skolemise poly2
>   subsumes p poly1 wp2
>   fvids <- freeVars [poly1, poly2]
>   if any (`elem` fvids) (map snd skolemSubs) then throwError $ CompilationError TypeChecker ("Polymorphic type was less general than was required (subsumption check failed)") p
>   else return ()

> subsumes :: SourcePos -> Polytype -> RhoType -> Environment () -- |-dsk* sigma <= rho
> subsumes p poly1@(TForAll _ _) wp2 = do -- rule SPEC
>   wp1 <- instantiate poly1
>   subsumes p wp1 wp2
> subsumes p t1 (TFunc targs2 tret2) = do -- rule FUN
>   (targs1, tret1) <- assertFunc p (length targs2) t1
>   subsumesFunc p targs1 tret1 targs2 tret2
> subsumes p (TFunc targs1 tret1) t2 = do -- rule FUN
>   (targs2, tret2) <- assertFunc p (length targs1) t2
>   subsumesFunc p targs1 tret1 targs2 tret2
> subsumes p (TList t1) (TList t2) = subsumes p t1 t2 -- propagate
> subsumes p (TTuple t1a t1b) (TTuple t2a t2b) = do -- propagate
>   subsumes p t1a t2a
>   subsumes p t1b t2b
> subsumes p mono1 mono2 = unify p mono1 mono2 -- rule MONO

> subsumesFunc :: SourcePos -> [Polytype] -> RhoType -> [Polytype] -> RhoType -> Environment () -- |-dsk* ({sigma1} -> sigma2) <= ({sigma3} -> rho4)
> subsumesFunc p targs1 tret1 targs2 tret2 = do -- rule FUN
>   zipWithM_ (\targ1 targ2 -> subsumesPoly p targ2 targ1) targs1 targs2
>   if (tret1 == TVoid) /= (tret2 == TVoid) then throwError $ CompilationError TypeChecker ("Void function cannot be used when a non-void return type is expected") p
>   else subsumes p tret1 tret2



===============================================================================
 Polytype handlers
===============================================================================

> quantify :: VId -> [VId] -> RhoType -> Environment ()
> quantify _ [] t = return ()
> quantify vid vids t@(TFunc _ _) = do
>   bvars <- mapM (\_ -> freshBoundVar) vids
>   apply (zip vids bvars)
>   t' <- convert t
>   replaceSub vid $ TForAll (map getvid bvars) t'
>   where getvid (TBound i) = i
> quantify vid vids (TList t) = quantify vid vids t
> quantify vid vids (TTuple t1 t2) = do 
>   internals1 <- typeVars [t1]
>   quantify vid (internals1 `intersect` vids) t1
>   internals2 <- typeVars [t2]
>   quantify vid (internals2 `intersect` vids) t2
> quantify _ _ t = return () -- monotype

> instantiate :: Polytype -> Environment RhoType
> instantiate (TForAll vids t) = do
>   insts <- mapM (\_ -> fresh) vids
>   return $ zip vids insts |-> t
> instantiate t = return t

> skolemise :: Polytype -> Environment ([(VId, VId)], RhoType) -- pr(sigma)
> skolemise (TForAll vids t) = do -- rule PRPOLY
>   skolems1 <- mapM (\_ -> freshSkolemVar) vids
>   (skolems2, t') <- skolemise (zip vids skolems1 |-> t)
>   return ((zip vids $ map vid skolems1) ++ skolems2, t')
>   where vid (TSkolem i) = i
> skolemise (TFunc targs tres) = do -- rule PRFUN
>   (skolems, tres) <- skolemise tres
>   return (skolems, TFunc targs tres)
> skolemise (TList t) = do -- propagate
>   (skolems, t) <- skolemise t
>   return (skolems, TList t)
> skolemise (TTuple t1 t2) = do -- propagate
>   (skolems1, t1) <- skolemise t1
>   (skolems2, t2) <- skolemise t2 -- since foralls bind locally, t2 is not affected by t1's foralls
>   return (skolems1 ++ skolems2, TTuple t1 t2)
> skolemise t = return ([], t) -- rule PRMONO

> assertFunc :: SourcePos -> Int -> RhoType -> Environment ([Polytype], RhoType)
> assertFunc p nargs (TFunc targs tret)
>   | length targs == nargs = return (targs, tret)
>   | otherwise = throwError $ CompilationError TypeChecker ("Cannot unify functions with mismatching numbers of arguments") p
> assertFunc p nargs t = do
>   vargs <- replicateM nargs fresh
>   vret <- fresh
>   addErrorDesc "Variable does not contain a function: " $ unify p t (TFunc vargs vret)
>   return (vargs, vret)

> freeVars :: [Polytype] -> Environment [VId] -- ftv for skolemised sigma
> freeVars ts = do
>   ts' <- mapM convert ts
>   return $ foldr (freeVarAccum []) [] ts'
>   where freeVarAccum bound (TBound i) acc 
>           | i `elem` bound = acc
>           | i `elem` acc = acc
>           | otherwise = i:acc
>         freeVarAccum bound (TSkolem i) acc
>           | i `elem` acc = acc
>           | otherwise = i:acc
>         freeVarAccum bound (TList t) acc          = freeVarAccum bound t acc
>         freeVarAccum bound (TTuple t1 t2) acc     = freeVarAccum bound t1 $ freeVarAccum bound t2 acc
>         freeVarAccum bound (TFunc targs tret) acc = foldr (freeVarAccum bound) acc (tret:targs)
>         freeVarAccum bound (TForAll vids t) acc   = freeVarAccum (vids ++ bound) t acc
>         freeVarAccum bound _ acc = acc

> typeVars :: [Polytype] -> Environment [VId] -- ftv
> typeVars ts = do
>   ts' <- mapM convert ts
>   return $ foldr typeVarAccum [] ts'
>   where typeVarAccum (TVar i) acc 
>           | i `elem` acc = acc
>           | otherwise = i:acc
>         typeVarAccum (TList t) acc          = typeVarAccum t acc
>         typeVarAccum (TTuple t1 t2) acc     = typeVarAccum t1 $ typeVarAccum t2 acc
>         typeVarAccum (TFunc targs tret) acc = foldr typeVarAccum acc (tret:targs)
>         typeVarAccum (TForAll vids t) acc   = typeVarAccum t acc
>         typeVarAccum _ acc = acc



===============================================================================
 Expected type handlers
===============================================================================

> assertType :: SourcePos -> Polytype -> Expected RhoType -> Environment () -- |-inst sigma <= rho
> assertType p t (Check texp) = do
>   t <- convert t
>   texp <- convert texp
>   subsumes p t texp
> assertType p t (Infer tv) = do
>   t <- convert t
>   t' <- instantiate t
>   unify p tv t'

> equivalent :: SourcePos -> Expected Type -> Expected Type -> Environment ()
> equivalent _ (Check _) _ = return ()
> equivalent p (Infer va) (Infer vb) = do
>   va <- convert va
>   vb <- convert vb
>   subsumesPoly p va vb
>   va <- convert va
>   vb <- convert vb
>   subsumesPoly p vb va

> propagateSubtype :: Expected Type -> Expected Type -> Environment ()
> propagateSubtype (Check _) _ = return ()
> propagateSubtype (Infer vsub) (Infer vexp) = do
>   vsub <- convert vsub
>   unify nP vexp vsub

> expectedSubtype :: Expected Type -> Environment (Expected Type)
> expectedSubtype (Check t) = return $ Check t
> expectedSubtype (Infer _) = do
>   v <- fresh
>   return $ Infer v



===============================================================================
 State substitution functions
===============================================================================

> apply :: SubList -> Environment ()
> apply subs = do 
>   s <- get
>   let (oldsubs, scopes, nextvar) = s
>   let appliedsubs = subs ++ [(i, subs |-> t) | (i,t) <- oldsubs]
>   put $ (appliedsubs, scopes, nextvar)

> convert :: Type -> Environment Type
> convert t = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   return $ subs |-> t

> replaceSub :: Int -> Type -> Environment ()
> replaceSub vid vt = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   let replacedsubs = replaceSub' subs vid vt
>   put (replacedsubs, scopes, nextvar)
>   where replaceSub' [] vid vt = [(vid,vt)]
>         replaceSub' ((i,t):subs) vid vt
>           | i == vid  = (i,vt) : subs
>           | otherwise = (i,t)  : (replaceSub' subs vid vt)


===============================================================================
 State variable functions
===============================================================================

> getVarType :: GramId -> Environment Type
> getVarType v = do
>   s <- get
>   let (subs, _, _) = s
>   i <- getVarId v
>   return $ subs |-> (TVar i)

> getVarId :: GramId -> Environment VId
> getVarId v = do
>   s <- get
>   let (_, scopes, _) = s
>   getVarId' v scopes
>   where getVarId' :: GramId -> [Scope] -> Environment VId
>         getVarId' (Id p var) [] = throwError $ CompilationError TypeChecker ("Variable out of scope: " ++ var) p
>         getVarId' vid@(Id p var) (scope:scopes) =
>           case lookup var scope of
>             Nothing -> getVarId' vid scopes
>             Just i  -> return i

> declareVar :: GramId -> Environment Type
> declareVar (Id p var) = do
>   s <- get
>   let (subs, scope:scopes, nextvar) = s
>   case lookup var scope of
>     Just i  -> if i >= 0 then throwError $ CompilationError TypeChecker ("Variable declared twice: " ++ var) p
>                else throwError $ CompilationError TypeChecker  ("Cannot override built-in function: " ++ var) p
>     Nothing -> do
>       let newscopes = if var /= "" then ((var, nextvar):scope):scopes else scope:scopes
>       put (subs, newscopes, nextvar+1)
>       return $ TVar nextvar

> fresh = declareVar (Id nP "")

> freshBoundVar :: Environment Type
> freshBoundVar = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   put (subs, scopes, nextvar+1)
>   return $ TBound nextvar

> freshSkolemVar :: Environment Type
> freshSkolemVar = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   put (subs, scopes, nextvar+1)
>   return $ TSkolem nextvar


===============================================================================
 State scope handlers
===============================================================================

> addToScope :: GramId -> VId -> Environment ()
> addToScope (Id p var) i = do
>   s <- get
>   let (subs, scope:scopes, nextvar) = s
>   case lookup var $ last $ scope:scopes of
>     Just i  -> if i < 0 then throwError $ CompilationError TypeChecker ("Cannot override built-in function: " ++ var) p
>                else return ()
>     Nothing -> return ()
>   case lookup var scope of
>     Just i  -> throwError $ CompilationError TypeChecker ("Variable declared twice: " ++ var) p
>     Nothing -> do
>       let newscopes = ((var,i):scope):scopes
>       put (subs, newscopes, nextvar)

> removeFromScope :: GramId -> Environment VId
> removeFromScope id@(Id _ i) = do
>   vid <- getVarId id
>   s <- get
>   let (subs, scopes, nextvar) = s
>   let newscopes = removeFromScope' i scopes
>   put (subs, newscopes, nextvar)
>   return vid
>   where removeFromScope' var (scope:scopes) = case lookup var scope of Just _  -> (filter (\x -> fst x /= var) scope) : scopes
>                                                                        Nothing -> scope : (removeFromScope' var scopes)

> pushScope :: Environment ()
> pushScope = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   put (subs, []:scopes, nextvar)

> popScope :: Environment Scope
> popScope = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   put (subs, tail scopes, nextvar)
>   return $ head scopes

> envTypes :: Environment [Type]
> envTypes = do
>   s <- get 
>   let (subs, scopes, nextvar) = s
>   return $ map (\vid -> subs |-> TVar vid) $ listVarIds [] scopes
>   where listVarIds _ [] = []
>         listVarIds acc (scope:scopes) = 
>           let (acc1, ids1) = listVarIds' acc scope in
>             ids1 ++ listVarIds acc1 scopes 
>         listVarIds' acc [] = (acc, [])
>         listVarIds' acc ((var,vid):scope)
>           | var `elem` acc = listVarIds' acc scope
>           | otherwise =
>             let (newacc, ids) = listVarIds' (var:acc) scope in
>               (newacc, vid:ids)



===============================================================================
 Grammar type handlers
===============================================================================

> convertGramType :: GramType -> Environment GramType
> convertGramType gt = do
>   t <- convertFromGramType gt
>   t <- convert t
>   return $ convertToGramType t

> convertFromGramType :: GramType -> Environment Type
> convertFromGramType (GramBasicType _ BoolType)    = return TBool
> convertFromGramType (GramBasicType _ CharType)    = return TChar
> convertFromGramType (GramBasicType _ IntType)     = return TInt
> convertFromGramType (GramTupleType _ t1 t2)       = do
>   t1 <- convertFromGramType t1 
>   t2 <- convertFromGramType t2
>   return $ TTuple t1 t2
> convertFromGramType (GramListType _ t)            = do
>   t <- convertFromGramType t
>   return $ TList t
> convertFromGramType (GramIdType id)               = do
>   readVarFromGramType id
> convertFromGramType (GramFunType _ (GramFunTypeAnnot targs tret))    = do
>   targs <- mapM convertFromGramType targs
>   tret <- convertFromGramRetType tret
>   return $ TFunc targs tret
> convertFromGramType (GramForAllType _ boundids t) = do
>   tbound <- mapM readVarFromGramType boundids
>   t <- convertFromGramType t
>   return $ TForAll (map (\(TBound i) -> i) tbound) t

> convertFromGramRetType :: GramRetType -> Environment Type
> convertFromGramRetType (GramVoidType _) = do return TVoid
> convertFromGramRetType (GramRetType t) = do convertFromGramType t

> readVarFromGramType :: GramId -> Environment Type -- note this storage method is safe because the lexer prohibits identifiers starting with underscores
> readVarFromGramType (Id _ id)
>   | isPrefixOf "_t" id =
>     case readMaybe (drop 2 id) :: Maybe Int of
>       Just vid -> return $ TVar vid
>       Nothing  -> fresh
>   | isPrefixOf "_v" id =
>     case readMaybe (drop 2 id) :: Maybe Int of
>       Just vid -> return $ TBound vid
>       Nothing  -> fresh
>   | isPrefixOf "_void" id = return TVoid
>   | otherwise = fresh

> convertToGramType :: Type -> GramType
> convertToGramType TBool                = GramBasicType nP BoolType
> convertToGramType TChar                = GramBasicType nP CharType
> convertToGramType TInt                 = GramBasicType nP IntType
> convertToGramType TVoid                = GramIdType (Id nP ("_void")) -- needed because return type variables are unified with void - ex.: "f(x){} g(){f(0);}"
> convertToGramType (TTuple t1 t2)       = GramTupleType nP (convertToGramType t1) (convertToGramType t2)
> convertToGramType (TList t)            = GramListType nP (convertToGramType t)
> convertToGramType (TVar i)             = GramIdType (Id nP ("_t" ++ (show i)))
> convertToGramType (TBound i)           = GramIdType (Id nP ("_v" ++ (show i)))
> convertToGramType (TFunc targs tret)   = GramFunType nP $ GramFunTypeAnnot (map convertToGramType targs) (convertToGramRetType tret)
> convertToGramType (TForAll tbound t)   = GramForAllType nP (map (\i -> Id nP $ "_v" ++ (show i)) tbound) (convertToGramType t)

> convertToGramRetType :: Type -> GramRetType
> convertToGramRetType TVoid = GramVoidType nP
> convertToGramRetType t = GramRetType $ convertToGramType t

> getExpPos :: GramExp -> SourcePos
> getExpPos (GramBool p _) = p
> getExpPos (GramChar p _) = p
> getExpPos (GramNum p _) = p
> getExpPos (GramEmptyList p) = p
> getExpPos (GramExpTuple p _ _) = p
> getExpPos (GramBinary p _ _ _) = p
> getExpPos (GramOverloadedBinary p _ _ _ _) = p
> getExpPos (GramUnary p _ _) = p
> getExpPos (GramExpId (Var (Id p _) _)) = p
> getExpPos (GramExpFunCall (GramFunCall (Id p _) _)) = p
> getExpPos (GramExpFunCall (GramOverloadedFunCall _ (Id p _) _)) = p

> unifyWith :: SourcePos -> [(String, Type)] -> Type -> GramType -> Environment ([(String, Type)], Type)
> unifyWith p insts v (GramBasicType _ BoolType) = do
>   unify p v TBool
>   return (insts, TBool)
> unifyWith p insts v (GramBasicType _ CharType) = do
>   unify p v TChar
>   return (insts, TChar)
> unifyWith p insts v (GramBasicType _ IntType) = do
>   unify p v TInt
>   return (insts, TInt)
> unifyWith p insts v (GramListType _ t) = do
>   vin <- fresh
>   (insts,t) <- unifyWith p insts vin t
>   unify p v $ TList t
>   return (insts, TList t)
> unifyWith p insts v (GramTupleType _ t1 t2) = do
>   v1 <- fresh
>   v2 <- fresh
>   (insts,t1) <- unifyWith p insts v1 t1
>   (insts,t2) <- unifyWith p insts v2 t2
>   unify p v $ TTuple t1 t2
>   return (insts, TTuple t1 t2)
> unifyWith p insts v (GramFunType _ (GramFunTypeAnnot [] (GramRetType tret))) = do
>   vret <- fresh
>   (insts,tret) <- unifyWith p insts vret tret
>   unify p v (TFunc [] tret)
>   return (insts, TFunc [] tret)
> unifyWith p insts v (GramFunType _ (GramFunTypeAnnot [] (GramVoidType _))) = do
>   unify p v (TFunc [] TVoid)
>   return (insts, TFunc [] TVoid)
> unifyWith p insts v (GramFunType p' (GramFunTypeAnnot (targ:targs) tret)) = do
>   varg <- fresh
>   vargs <- fresh
>   (insts,targ)  <- unifyWith p insts varg targ
>   (insts,tfargs) <- unifyWith p insts vargs (GramFunType p' (GramFunTypeAnnot targs tret))
>   let TFunc targs tret = tfargs
>   unify p v (TFunc (targ:targs) tret)
>   return (insts, TFunc (targ:targs) tret)
> unifyWith p insts v (GramForAllType _ bound t) = do
>   vbound <- mapM (\_ -> freshBoundVar) bound
>   vt <- fresh
>   let newinsts = (map (\(Id _ i) -> i) bound) `zip` vbound ++ insts
>   (finsts, t) <- unifyWith p newinsts vt t
>   let forallt = TForAll (map (\(TBound i) -> i) vbound) t
>   unify p v forallt
>   return (finsts, forallt)
> unifyWith p insts v (GramIdType (Id _ i)) = 
>   case lookup i insts of
>     Just t -> do
>       unify p v t
>       return (insts, t)
>     Nothing -> do
>       nv <- fresh
>       unify p v nv
>       return ((i, nv):insts, nv)



===============================================================================
 Error handlers
===============================================================================

> addErrorDesc :: String -> Environment t -> Environment t
> addErrorDesc s = mapStateT (wrapError s)
>   where wrapError _ (Right r) = Right r
>         wrapError s (Left (CompilationError _ msg p)) = Left $ CompilationError TypeChecker (s ++ msg) p

> replaceErrorType :: String -> String -> Environment t -> Environment t
> replaceErrorType frm to = mapStateT (wrapError frm to)
>   where wrapError _ _ (Right r) = Right r
>         wrapError frm to (Left (CompilationError _ msg p))
>           | isPrefixOf frm msg = Left $ CompilationError TypeChecker to  p
>           | otherwise  = Left $ CompilationError TypeChecker msg p



===============================================================================
 Initialisation functions
===============================================================================

> initEnv :: EnvType
> initEnv = ([(-2, tprint), (-4, tprintln), (-6, tdisplay), (-8, tempty), (-9, tchr), (-10, tord), (-11, terror)],
>            [[("print", -2), ("println", -4), ("display", -6), ("isEmpty", -8), ("chr", -9), ("ord", -10), ("error", -11)]],0)
>   where tprint = TForAll [-1] $ TFunc [TBound (-1)] TVoid
>         tprintln = TForAll [-3] $ TFunc [TBound (-3)] TVoid
>         tdisplay = TForAll [-5] $ TFunc [TBound (-5)] TVoid
>         tempty = TForAll [-7] $ TFunc [TList $ TBound (-7)] TBool
>         tchr = TFunc [TInt] TChar
>         tord = TFunc [TChar] TInt
>         terror = TFunc [TList TChar] TVoid

> nP :: SourcePos
> nP = newPos "<internal>" 0 0

