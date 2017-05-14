> module PolyChecker where

> import Control.Monad.Except
> import Control.Monad.State
> import Data.Ord (comparing)
> import Data.List ((\\), find, nub, sortBy)
> import Dependency
> import Grammar
> import Printer (printGram)
> import Text.Parsec.Pos (newPos, SourcePos)
> import Text.Read (readMaybe)
> import Token 

> data Type = TBool | TInt | TChar | TVoid | TTuple Type Type | TList Type | TFunc [Type] Type | TScheme [Type] Type | TVar Int | TFree Int
>   deriving (Show, Eq, Ord)

> data RetType = DoesNotReturn | SometimesReturns Type | AlwaysReturns Type
>   deriving (Show, Eq)

> type SubList = [(Int, Type)]
> type Scope = [(String, Int)]
> type EnvType = (SubList, [Scope], Int)
> type TypeError = (String, SourcePos)

> type Environment = StateT EnvType (Either TypeError)


===============================================================================
===============================================================================
PolyTypeChecker

Performs type checking and inference, and parse tree annotation.

This consists of the following major stages:
1. dependency analysis
2. block-wise type inference and initial tree decoration
3. post-inference decoration

1. Dependency analysis
See dependency.lhs. This provides definition order independence and 
divides the tree into mutually recursive blocks.

2. Block-wise type inference and initial tree decoration
Applied per mutually recursive block, and divided into the following stages:

2a. Initial variable/function declarations
    Names are registered in the scope and type variables are set up
	for variables, function arguments and return values,
	so that blocks' type inference is order-independent.
2b. Type inference and initial tree decoration
    Entire tree is traversed and type inference based on algorithm M,
	extended as necessary to functions and by-reference variables is performed.
	Tree decoration is performed on function calls and overloaded operators 
	during traversal, meaning type variables
	are left in annotations when still unresolved.
2c. Generalisation and global type annotations
    Functions with unresolved argument types are generalised
	to polymorphic type schemes, such as in e.g. the identity function.
	This can only happen after the entire mutually recursive block has been
	processed by type inference, hence the need for a separate stage. 
	Furthermore, variables and function types are annotated with their types.

3. Post-inference decoration
Traverses the tree once more, to finalise tree decorations by substituting in
all known type resolutions. This is necessary for order-independence, as
stage 2 annotates variables, expressions and function calls sequentially, 
possibly before type information becomes available.

Notes:
Called with inferProg, returning only the decorated tree.
See note above inferProg for a way to display found type information and
the annotated program.
Does not support higher-order functions.
Does not support assignment of functions to variables
(aliasing, e.g. "var p = isEmpty; var true = p([])").
===============================================================================
===============================================================================


===============================================================================
 Known issues / improvements
===============================================================================

TBD: char vs int? currently arithmetic works only for integers.
     char arithmetic is nice ('0' + i), but chars are to be printed as chars.
	 so TChar cannot be removed.. how to solve? 
	 TChar and TInt cannot simply unify because '0'+2 or '7'-'0' needs to be
	 TChar, but 1+1 needs to be TInt in GramBinary Plus
TODO: disallow custom type annotations for variable declarations 




===============================================================================
 Top-level structure
===============================================================================

N.B. returning Right $ (cleanEnv env, printGram annot) instead of Right annot
returns (EnvType, String) with a cleaned-up environment containing
types for variables and functions, and the pretty-printed parse tree
with type annotations in /*comments*/

> inferProg :: [GramDecl] -> Either TypeError [GramDecl]
> inferProg decls =
>   let declBlocks = Dependency.dependencyBlocks decls in
>   case runStateT (addErrorDesc "[TYPE ERROR] " $ inference declBlocks) initEnv of
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
>   nxt <- counter
>   inferDeclsHeader ds
>   ds <- inferDeclsBody ds
>   ds <- inferDeclsPost nxt ds
>   return ds
>   where inferDeclsHeader [] = return ()
>         inferDeclsHeader (decl:decls) = do
>           case decl of
>             GramDeclVar vardecl -> inferVarDeclHeader vardecl
>             GramDeclFun fundecl -> inferFunDeclHeader fundecl 
>           inferDeclsHeader decls
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
>         inferDeclsPost _ [] = return []
>         inferDeclsPost nxt (decl:decls) = do
>           decl <- case decl of
>             GramDeclVar vardecl -> do
>               vardecl <- inferVarDeclPost vardecl
>               return $ GramDeclVar vardecl
>             GramDeclFun fundecl -> do
>               fundecl <- inferFunDeclPost nxt fundecl
>               return $ GramDeclFun fundecl
>           decls <- inferDeclsPost nxt decls
>           return $ decl:decls

> checkRecursiveVars :: [GramDecl] -> Environment () -- forbids otherwise allowed "var flip = 0:flop; var flop = 1:flip;"
> checkRecursiveVars decls = case find isVar decls of
>   Nothing   -> return ()
>   Just decl -> 
>     case () of 
>       _ | length decls > 1 ->
>             let Id p i = getId decl in
>               throwError ("Mutually recursive variable definitions are not allowed: " ++ i, p)
>         | otherwise -> return ()
>   where isVar (GramDeclVar _) = True
>         isVar _ = False
>         getId (GramDeclVar (GramVarDeclType _ (GramVarDeclTail id _))) = id
>         getId (GramDeclVar (GramVarDeclVar (GramVarDeclTail id _))) = id



===============================================================================
 Global declarations - stage 2a
===============================================================================

> inferVarDeclHeader :: GramVarDecl -> Environment Type
> inferVarDeclHeader vardecl = declareVar $ getName vardecl
>   where getName (GramVarDeclType _ (GramVarDeclTail vid _)) = vid
>         getName (GramVarDeclVar    (GramVarDeclTail vid _)) = vid

> inferFunDeclHeader :: GramFuncDecl -> Environment Type
> inferFunDeclHeader (GramFuncDecl id@(Id p i) (GramFuncDeclTail fargs [] stmts)) = do
>   vfun <- declareVar id
>   arglist <- listArgs fargs
>   vret <- fresh
>   let tfun = TFunc arglist vret
>   unify p vfun tfun
>   convert vfun
>   where listArgs [] = return []
>         listArgs (_:fargs) = do
>           v <- fresh
>           arglist <- listArgs fargs
>           return $ v:arglist
> inferFunDeclHeader (GramFuncDecl id@(Id p i) (GramFuncDeclTail fargs [GramFunType ftypes tret] stmts)) = do
>   vfun <- declareVar id
>   vargs <- listArgs fargs
>   vret <- fresh
>   (arglist, insts) <- listArgTypes id [] vargs ftypes
>   tret <- retType insts vret tret
>   let tfun = TFunc arglist tret
>   unify p vfun tfun
>   convert vfun
>   where listArgs [] = return []
>         listArgs (_:fargs) = do
>           v <- fresh
>           arglist <- listArgs fargs
>           return $ v:arglist
>         listArgTypes _ insts [] [] = return ([], insts)
>         listArgTypes id insts (v:vars) [GramFTypes t ftypes] = do
>           (v, newinsts) <- unifyWith insts v t
>           (arglist, finsts) <- listArgTypes id newinsts vars ftypes
>           return $ (v:arglist, finsts)
>         listArgTypes id@(Id pos i) _ _ _ = throwError ("Mismatching number of arguments in type signature: " ++ i, pos)
>         retType insts v (GramRetType t)  = do
>           (tret,_) <- unifyWith insts v t
>           return tret
>         retType insts v (GramVoidType p) = do
>           unify p v TVoid
>           return TVoid



===============================================================================
 High-level type inference - stage 2b
===============================================================================

> inferVarDeclBody :: GramVarDecl -> Environment GramVarDecl
> inferVarDeclBody vardecl = case vardecl of
>   GramVarDeclType t vardecltail -> do
>     (v, vardecltail) <- inferVarDeclTail vardecltail
>     unifyWith [] v t
>     return $ GramVarDeclType t vardecltail
>   GramVarDeclVar vardecltail    -> do
>     (_, vardecltail) <- inferVarDeclTail vardecltail
>     return $ GramVarDeclVar vardecltail
>   where inferVarDeclTail (GramVarDeclTail id@(Id p i) e) = do
>           v <- getVarType id
>           vid <- removeFromScope id
>           e <- replErr i $ inferExpr e v
>           v <- convert v
>           addToScope id vid
>           if occurs vid v then throwError ("Recursive variable definition detected: " ++ i, p)
>           else do
>             if isFunc v then throwError ("Variables cannot contain functions: " ++ i, p)
>             else return (v, GramVarDeclTail id e)
>         isFunc (TFunc _ _)   = True
>         isFunc (TScheme _ _) = True
>         isFunc _ = False
>         replErr var = replaceErrorDesc ("Variable out of scope: " ++ var) ("Recursive variable definition detected: " ++ var)

> inferFunDeclBody :: GramFuncDecl -> Environment GramFuncDecl
> inferFunDeclBody (GramFuncDecl id@(Id p i) (GramFuncDeclTail fargs ftypes stmts)) = do
>   fid <- getVarId id
>   tfun <- getVarType id
>   let TFunc arglist vret = tfun
>   pushScope
>   declareArgs fargs (fid+1)
>   pushScope
>   (tret,stmts) <- inferStmtBlock stmts
>   popScope
>   popScope
>   case tret of
>     DoesNotReturn      -> unify p vret TVoid
>     AlwaysReturns t    -> unify p vret t
>     SometimesReturns t -> do
>       if t == TVoid then unify p vret TVoid
>       else throwError ("Non-void function contains non-returning code paths: " ++ i, p)
>   return $ GramFuncDecl id $ GramFuncDeclTail fargs ftypes stmts
>   where declareArgs [] _ = return ()
>         declareArgs (aid:fargs) ai = do
>           addToScope aid ai
>           declareArgs fargs (ai+1)

> inferStmtBlock :: [GramStmt] -> Environment (RetType, [GramStmt])
> inferStmtBlock [] = return (DoesNotReturn, [])
> inferStmtBlock (stmt:stmts) = do
>   (p, tret, stmt) <- inferStmt stmt
>   case tret of 
>     DoesNotReturn      -> do
>       (tret, stmts) <- inferStmtBlock stmts
>       return (tret, stmt:stmts)
>     AlwaysReturns t1   ->
>       if null stmts then return $ (AlwaysReturns t1, [stmt])
>       else throwError ("Unreachable code detected", p)
>     SometimesReturns t1 -> do
>       (t2, stmts) <- inferStmtBlock stmts
>       t1 <- convert t1
>       case t2 of
>         DoesNotReturn -> return (SometimesReturns t1, stmt:stmts)
>         AlwaysReturns t2 -> do
>           addErrorDesc "Inconsistent return types: " $ unify p t2 t1
>           t2 <- convert t2
>           return (AlwaysReturns t2, stmt:stmts)
>         SometimesReturns t2 -> do
>           addErrorDesc "Inconsistent return types: " $ unify p t2 t1
>           t2 <- convert t2
>           return (SometimesReturns t2, stmt:stmts)

> inferStmt :: GramStmt -> Environment (SourcePos, RetType, GramStmt)
> inferStmt (GramIf p cond tr fa) = do
>   cond <- addErrorDesc "Non-boolean expression used as branch condition: " $ inferExpr cond TBool
>   pushScope
>   (t1, tr) <- inferStmtBlock tr
>   popScope 
>   pushScope
>   (t2, fa) <- inferStmtBlock fa
>   popScope
>   let stmt = GramIf p cond tr fa
>   if t1 == DoesNotReturn then (
>     if t2 == DoesNotReturn then return (p, DoesNotReturn, stmt)
>     else return (p, SometimesReturns $ typeof t2, stmt) )
>   else if t2 == DoesNotReturn then return (p, SometimesReturns $ typeof t1, stmt)
>     else do
>       addErrorDesc "Inconsistent return types: " $ unify p (typeof t1) (typeof t2)
>       tret <- convert $ typeof t1
>       if (always t1 && always t2) then return (p, AlwaysReturns tret, stmt)
>       else return (p, SometimesReturns tret, stmt)
>   where typeof (SometimesReturns t) = t
>         typeof (AlwaysReturns t) = t
>         always (AlwaysReturns _) = True
>         always _ = False
> inferStmt (GramWhile p cond lp) = do
>   cond <- addErrorDesc "Non-boolean expression used as loop condition: " $ inferExpr cond TBool
>   pushScope
>   (tret, lp) <- inferStmtBlock lp
>   popScope
>   let stmt = GramWhile p cond lp
>   case tret of
>     DoesNotReturn      -> return (p, DoesNotReturn, stmt)
>     SometimesReturns t -> return (p, SometimesReturns t, stmt)
>     AlwaysReturns t    -> return (p, SometimesReturns t, stmt)
> inferStmt (GramReturn p ret) =
>   case ret of
>     Nothing -> return $ (p, AlwaysReturns TVoid, GramReturn p Nothing)
>     Just e  -> do
>       v <- fresh
>       e <- inferExpr e v
>       v <- convert v
>       return $ (p, AlwaysReturns v, GramReturn p (Just e))
> inferStmt (GramFunVarDecl vardecl) = do
>   inferVarDeclHeader vardecl
>   vardecl <- inferVarDeclBody vardecl
>   vardecl <- inferVarDeclPost vardecl
>   return (getPos vardecl, DoesNotReturn, GramFunVarDecl vardecl)
>   where getPos (GramVarDeclType _ (GramVarDeclTail (Id p i) _)) = p
>         getPos (GramVarDeclVar    (GramVarDeclTail (Id p i) _)) = p
> inferStmt (GramAttr p (Var id@(Id pos i) fields) e) = do
>   vid <- getVarId id
>   t <- getVarType id
>   if vid < 0 then throwError ("Cannot override built-in function: " ++ i, pos)
>   else do
>     if isFunc t then throwError ("Functions cannot be assigned to: " ++ i, pos)
>     else do
>       tfield <- fresh
>       inferField fields p tfield t
>       tfield <- convert tfield
>       e <- addErrorDesc ("Variable assignment failed (" ++ i ++ "): ") $ inferExpr e tfield
>       return (p, DoesNotReturn, GramAttr p (Var id fields) e)
>   where isFunc (TFunc _ _)   = True
>         isFunc (TScheme _ _) = True
>         isFunc _ = False
> inferStmt (GramStmtFunCall funcall) = do
>   (p, _, funcall) <- inferFunCall funcall
>   return (p, DoesNotReturn, GramStmtFunCall funcall)


===============================================================================
 Type inference algorithm M (Damas-Hindley-Milner) - stage 2b
===============================================================================

> inferExpr :: GramExp -> Type -> Environment GramExp
> inferExpr orig@(GramBool p _) t = do
>   unify p t TBool
>   return orig
> inferExpr orig@(GramChar p _) t = do -- for stuff like int+'0', maybe treat chars as int?
>   unify p t TChar
>   return orig
> inferExpr orig@(GramNum p _)  t = do
>   unify p t TInt
>   return orig
> inferExpr orig@(GramEmptyList p) t = do
>   v <- fresh
>   unify p t (TList v)
>   return orig
> inferExpr (GramExpTuple p e1 e2) t = do
>   v1 <- fresh
>   v2 <- fresh
>   e1 <- inferExpr e1 v1
>   e2 <- inferExpr e2 v2
>   tup <- convert (TTuple v1 v2)
>   unify p t tup
>   return $ GramExpTuple p e1 e2
> inferExpr (GramOverloadedBinary p _ op e1 e2) t = inferExpr (GramBinary p op e1 e2) t
> inferExpr (GramBinary p op e1 e2) t = do
>   (t1, t2, tret, ol) <- opType op
>   e1 <- inferExpr e1 t1
>   t2 <- convert t2
>   e2 <- inferExpr e2 t2
>   unify p t tret
>   t1 <- convert t1
>   if ol then return $ GramOverloadedBinary p (convertToGramType t1) op e1 e2
>   else return $ GramBinary p op e1 e2
> inferExpr (GramUnary p op e) t = do
>   (tel, _, tret, _) <- opType op
>   e <- inferExpr e tel
>   unify p t tret
>   return $ GramUnary p op e
> inferExpr orig@(GramExpId (Var vid@(Id p i) fields)) t = do
>   vart <- getVarType vid
>   inferField fields p t vart
>   return orig
> inferExpr (GramExpFunCall funcall) t = do
>   (p,tret,funcall) <- inferFunCall funcall
>   if tret == TVoid then throwError ("Void function used in an expression", p)
>   else addErrorDesc "Function return value used incorrectly: " $ unify p t tret
>   return $ GramExpFunCall funcall

> inferFunCall :: GramFunCall -> Environment (SourcePos, Type, GramFunCall)
> inferFunCall (GramOverloadedFunCall _ id args) = inferFunCall (GramFunCall id args)
> inferFunCall orig@(GramFunCall id@(Id p i) args) = do
>   tfun <- getVarType id
>   assertFunc tfun id
>   (tfun, isScheme) <- instantiate tfun
>   let TFunc argtypes tret = tfun
>   (args, argtypes) <- inferArgs id args argtypes
>   tret <- convert tret
>   if isScheme then return (p, tret, GramOverloadedFunCall argtypes id args)
>   else return (p, tret, GramFunCall id args)
>   where inferArgs _ [] [] = return ([],[])
>         inferArgs id (e:es) (targ:targs) = do
>           e <- addErrorDesc "Given argument has wrong type: " $ inferExpr e targ
>           (es,targs) <- inferArgs id es targs
>           targ <- convert targ
>           return (e:es, convertToGramType targ : targs)
>         inferArgs (Id pos i) _ _ = throwError ("Mismatching number of arguments given to: " ++ i, pos)
>         assertFunc :: Type -> GramId -> Environment ()
>         assertFunc (TFunc _ _) _ = return ()
>         assertFunc (TScheme _ _) _ = return ()
>         assertFunc (TVar _) (Id pos i) = throwError ("Trying to call a variable as a function (higher-order type inference is not supported): " ++ i, pos) -- change for HOF
>         assertFunc _ (Id pos i) = throwError ("Trying to call a non-function variable as a function: " ++ i, pos)

> inferField :: [GramField] -> SourcePos -> Type -> Type -> Environment ()
> inferField [] p tret vart = unify p tret vart
> inferField [First p fields] _ tret vart = do
>   v1 <- fresh
>   v2 <- fresh
>   unify p (TTuple v1 v2) vart
>   v1 <- convert v1
>   inferField fields p tret v1
> inferField [Second p fields] _ tret vart = do
>   v1 <- fresh
>   v2 <- fresh
>   unify p (TTuple v1 v2) vart
>   v2 <- convert v2
>   inferField fields p tret v2
> inferField [Head p fields] _ tret vart = do
>   v <- fresh
>   unify p (TList v) vart
>   v <- convert v
>   inferField fields p tret v
> inferField [Tail p fields] _ tret vart = do
>   v <- fresh
>   unify p (TList v) vart
>   v <- convert v
>   inferField fields p tret (TList v)

> opType :: Operation -> Environment (Type, Type, Type, Bool)
> opType op
>   | op `elem` [Minus, Plus, Times, Division, Mod] = return (TInt, TInt, TInt, False)
>   | op `elem` [LessThan, LessOrEqual, GreaterThan, GreaterOrEqual] = return (TInt, TInt, TBool, False)
>   | op `elem` [LogicalOr, LogicalAnd, LogicalNot] = return (TBool, TBool, TBool, False)
>   | op `elem` [Equals, Different] = do { v <- fresh; return (v, v, TBool, True) }
>   | op == ListConst = do { v <- fresh; return (v, TList v, TList v, False) } -- may need to be annotated depending on code gen


===============================================================================
 Unification algorithm U - stage 2b
===============================================================================

> unify :: SourcePos -> Type -> Type -> Environment ()
> unify p t1 t2 = do 
>   t1 <- convert t1
>   t2 <- convert t2
>   sub <- lift $ unification p t1 t2
>   apply p sub

> unification :: SourcePos -> Type -> Type -> Either TypeError SubList
> unification _ TVoid TVoid  = Right []
> unification _ TChar TChar  = Right []
> unification _ TInt TInt    = Right []
> unification _ TBool TBool  = Right []
> unification p (TVar i) t
>   | t == (TVar i)          = Right []
>   | occurs i t             = Left ("Recursive type detected", p)
>   | otherwise              = Right [(i, t)]
> unification p t (TVar i)
>   | t == (TVar i)          = Right []
>   | occurs i t             = Left ("Recursive type detected", p)
>   | otherwise              = Right [(i, t)]
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
> unification p t1 t2 = Left ("Can't unify types " ++ show t1 ++ " and " ++ show t2, p)

> occurs :: Int -> Type -> Bool
> occurs i (TTuple ta tb)          = (occurs i ta) || (occurs i tb)
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
> (|->) _ t                     = t

> infixr 3 |->



===============================================================================
 Global-level annotations - stage 2c
===============================================================================

> inferVarDeclPost :: GramVarDecl -> Environment GramVarDecl
> inferVarDeclPost (GramVarDeclVar vartail@(GramVarDeclTail id e)) = do
>   t <- getVarType id
>   return $ GramVarDeclType (convertToGramType t) vartail
> inferVarDeclPost vardecl = return vardecl -- if type already given

> inferFunDeclPost :: Int -> GramFuncDecl -> Environment GramFuncDecl
> inferFunDeclPost nxt (GramFuncDecl id (GramFuncDeclTail fargs _ stmts)) = do
>   generalise nxt id
>   tfun <- getVarType id
>   let ftypes = [funType tfun]
>   return $ GramFuncDecl id (GramFuncDeclTail fargs ftypes stmts)
>   where funType (TFunc targs tret)   = GramFunType (toFTypes targs) (retType tret)
>         funType (TScheme targs tret) = GramFunType (toFTypes targs) (retType tret)
>         toFTypes = (foldr (\t1 t2 -> [GramFTypes t1 t2]) []) . (map convertToGramType)
>         retType TVoid = GramVoidType nP
>         retType t = GramRetType $ convertToGramType t


===============================================================================
 Type scheme handlers - stage 2c
===============================================================================

> generalise :: Int -> GramId -> Environment ()
> generalise bstart id@(Id p i) = do
>     (subs, scopes, nextvar) <- get
>     fid  <- getVarId id
>     tfun <- getVarType id
>     if all (<bstart) $ instantiated subs tfun then return ()
>     else do
>       if (not $ retInstantiated tfun) then throwError ("Cannot infer type of non-terminating function: " ++ i, p)
>       else do
>         let newsubs = replaceSub subs fid $ scheme bstart tfun
>         put (newsubs, scopes, nextvar)
>   where retInstantiated (TFunc targs (TVar i)) = occurs i (TFunc targs TVoid)
>         retInstantiated (TFunc _ _) = True -- needs to be changed for HOF (e.g., for (.))
>         replaceSub [] fid tscheme = [(fid,tscheme)]
>         replaceSub ((i,t):subs) fid tscheme
>           | i == fid  = (i,tscheme) : subs
>           | otherwise = (i,t) : (replaceSub subs fid tscheme)
>         scheme bstart (TFunc targs tret) = TScheme (map (quantify bstart) targs) (quantify bstart tret)
>         quantify bstart t@(TVar i)
>           | i >= bstart = TFree i
>           | otherwise = t
>         quantify bstart (TList t) = TList $ quantify bstart t
>         quantify bstart (TTuple t1 t2) = TTuple (quantify bstart t1) (quantify bstart t2)
>         quantify _ t = t

> instantiate :: Type -> Environment (Type, Bool)
> instantiate (TScheme targs tret) = do
>   vfun <- fresh
>   (args,insts) <- instantiateArgs [] targs
>   (ret,_) <- instantiateArg insts tret
>   let tfun = TFunc args ret
>   unify nP vfun tfun
>   return (tfun, True)
>   where instantiateArgs :: [(Type,Type)] -> [Type] -> Environment ([Type], [(Type,Type)])
>         instantiateArgs insts [] = return ([], insts)
>         instantiateArgs insts (t:ts) = do
>           (newt, newinsts) <- instantiateArg insts t
>           (tlist,finsts) <- instantiateArgs newinsts ts 
>           return (newt:tlist, finsts)
>         instantiateArg :: [(Type,Type)] -> Type -> Environment (Type, [(Type,Type)])
>         instantiateArg insts v@(TFree i) =
>           case lookup v insts of
>             Just t  -> return (t, insts)
>             Nothing -> do
>               t <- fresh
>               let newinsts = (v,t) : insts
>               return (t, newinsts)
>         instantiateArg insts (TList t) = do
>           (newt, newinsts) <- instantiateArg insts t
>           return (TList newt, newinsts)
>         instantiateArg insts (TTuple t1 t2) = do
>           (newt1, newinsts1) <- instantiateArg insts t1
>           (newt2, newinsts2) <- instantiateArg newinsts1 t2
>           return (TTuple newt1 newt2, newinsts2)
>         instantiateArg insts t = return (t, insts)
> instantiate t = return (t, True)

 internals :: EnvType -> String -> Type -> [Int]
 internals (subs, scopes, nextvar) fid v = typerefs \\ externals
   where typerefs = instantiated subs v
         externals = concat [instantiated subs (TVar i) | (id,i) <- scope, id /= fid]
         scope = last scopes

> instantiated :: SubList -> Type -> [Int]
> instantiated subs (TFunc [] tret) = instantiated subs tret
> instantiated subs (TFunc (targ:targs) tret) = nub $ (instantiated subs targ) ++ (instantiated subs $ TFunc targs tret)
> instantiated subs (TList t) = instantiated subs t
> instantiated subs (TTuple t1 t2) = nub $ (instantiated subs t1) ++ (instantiated subs t2)
> instantiated subs (TVar i) = case lookup i subs of
>   Just t  -> instantiated subs t
>   Nothing -> [i]
> instantiated _ _ = []



===============================================================================
Tree post-decoration - stage 3
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
> postDecorateVarDecl (GramVarDeclType t (GramVarDeclTail id e)) = do
>   t <- convertGramType t
>   e <- postDecorateExpr e
>   return $ GramVarDeclType t $ GramVarDeclTail id e

> postDecorateFunDecl :: GramFuncDecl -> Environment GramFuncDecl
> postDecorateFunDecl (GramFuncDecl id (GramFuncDeclTail fargs [GramFunType ftypes tret] stmts)) = do
>   stmts <- postDecorateBlock stmts
>   ftypes <- convertFTypes ftypes
>   tret <- convertRetType tret
>   let funtype = GramFunType ftypes tret
>   return $ GramFuncDecl id $ GramFuncDeclTail fargs [funtype] stmts
>   where convertFTypes [] = return []
>         convertFTypes [GramFTypes t ftypes] = do
>           t <- convertGramType t
>           ftypes <- convertFTypes ftypes
>           return [GramFTypes t ftypes]
>         convertRetType (GramRetType t) = do
>           t <- convertGramType t
>           return $ GramRetType t
>         convertRetType (GramVoidType p) = return $ GramVoidType p
> postDecorateFunDecl a = return a

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
>   t <- convert $ convertFromGramType t
>   return $ GramOverloadedBinary p (convertToGramType t) op e1 e2
> postDecorateExpr (GramUnary p op e) = do
>   e <- postDecorateExpr e
>   return $ GramUnary p op e
> postDecorateExpr (GramExpFunCall funcall) = do
>   (tret,i,p,funcall) <- postDecorateFunCall funcall
>   if tret == TVoid then throwError ("Void function used in an expression: " ++ i, p) -- catches otherwise uncaught "f() { return g(); } g() { f(); }"
>   else return $ GramExpFunCall funcall
> postDecorateExpr e = return e

> postDecorateFunCall :: GramFunCall -> Environment (Type, String, SourcePos, GramFunCall)
> postDecorateFunCall funcall = case funcall of
>   GramFunCall id@(Id p i) es -> do
>     es <- postDecorateExprs es
>     tret <- retType id
>     return (tret, i, p, GramFunCall id es)
>   GramOverloadedFunCall ts id@(Id p i) es -> do
>     ts <- postDecorateTypes ts
>     es <- postDecorateExprs es
>     tret <- retType id
>     return (tret, i, p, GramOverloadedFunCall ts id es)
>   where postDecorateExprs [] = return []
>         postDecorateExprs (e:es) = do
>           e <- postDecorateExpr e
>           es <- postDecorateExprs es
>           return $ e:es
>         postDecorateTypes [] = return []
>         postDecorateTypes (t:ts) = do
>           t <- convertGramType t
>           ts <- postDecorateTypes ts
>           return $ t : ts
>         retType id = do
>           tfun <- getVarType id
>           tret <- case tfun of 
>             TScheme targs tret -> convert tret
>             TFunc targs tret   -> convert tret
>           return tret



===============================================================================
 State substitution functions
===============================================================================

> apply :: SourcePos -> SubList -> Environment ()
> apply p subs = do 
>   s <- get
>   let (oldsubs, scopes, nextvar) = s
>   let appliedsubs = subs ++ [(i, subs |-> t) | (i,t) <- oldsubs]
>   put $ (appliedsubs, scopes, nextvar)

> convert :: Type -> Environment Type
> convert t = do
>   s <- get
>   let (subs, scopes, nextvar) = s
>   return $ subs |-> t

> resolve :: EnvType -> EnvType
> resolve (subs, scopes, nextvar) = 
>   let newsubs = resolve' subs subs in 
>     (newsubs, scopes, nextvar)
>   where resolve' _ [] = []
>         resolve' subs ((i,TVar j):ts) =
>           case lookup j subs of
>             Just t  -> resolve' subs ((i,t):ts)
>             Nothing -> (i,TVar j) : resolve' subs ts
>         resolve' subs ((i,t):ts) = (i,t) : resolve' subs ts


===============================================================================
 State variable functions
===============================================================================

> getVarType :: GramId -> Environment Type
> getVarType v = do
>   s <- get
>   let (subs, _, _) = s
>   i <- getVarId v
>   return $ subs |-> (TVar i)

> getVarId :: GramId -> Environment Int
> getVarId v = do
>   s <- get
>   let (_, scopes, _) = s
>   getVarId' v scopes
>   where getVarId' :: GramId -> [Scope] -> Environment Int
>         getVarId' (Id p var) [] = throwError ("Variable out of scope: " ++ var, p)
>         getVarId' vid@(Id p var) (scope:scopes) =
>           case lookup var scope of
>             Nothing -> getVarId' vid scopes
>             Just i  -> return i

> declareVar :: GramId -> Environment Type
> declareVar (Id p var) = do
>   s <- get
>   let (subs, scope:scopes, nextvar) = s
>   case lookup var scope of
>     Just i  -> if i >= 0 then throwError ("Variable declared twice: " ++ var, p)
>                else throwError ("Cannot override built-in function: " ++ var, p)
>     Nothing -> do
>       let newscopes = if var /= "" then ((var, nextvar):scope):scopes else scope:scopes
>       put (subs, newscopes, nextvar+1)
>       return $ TVar nextvar

> fresh = declareVar (Id nP "")


===============================================================================
 State scope handlers
===============================================================================

> addToScope :: GramId -> Int -> Environment ()
> addToScope (Id p var) i = do
>   s <- get
>   let (subs, scope:scopes, nextvar) = s
>   case lookup var scope of
>     Just i  -> if i >= 0 then throwError ("Variable declared twice: " ++ var, p)
>                else throwError ("Cannot override built-in function: " ++ var, p)
>     Nothing -> do
>       let newscopes = ((var,i):scope):scopes
>       put (subs, newscopes, nextvar)

> removeFromScope :: GramId -> Environment Int
> removeFromScope id@(Id p i) = do
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

> cleanEnv :: EnvType -> EnvType
> cleanEnv (subs, scopes, nextvar) =
>   let newsubs = sortBy (comparing fst) $ globalSubs subs (head scopes) in
>   (newsubs, scopes, nextvar)
>   where globalSubs _ [] = []
>         globalSubs subs ((_,i):scope) =
>           case lookup i subs of
>             Just t  -> (i,t)       : globalSubs subs scope
>             Nothing -> (i, TVar i) : globalSubs subs scope

> counter :: Environment Int
> counter = do
>   (_,_,i) <- get
>   return i


===============================================================================
 Grammar type handlers
===============================================================================

> unifyWith :: [(String,Type)] -> Type -> GramType -> Environment (Type, [(String,Type)])
> unifyWith insts v (GramIdType (Id p id)) = case lookup id insts of
>   Just inst -> do
>     unify p v inst
>     return (inst, insts)
>   Nothing   -> do
>     let newinsts = (id,v) : insts
>     return (v, newinsts)
> unifyWith insts v (GramTupleType p t1 t2) = do
>   v1 <- fresh
>   v2 <- fresh
>   (v1, newinsts1) <- unifyWith insts v1 t1
>   (v2, newinsts2) <- unifyWith newinsts1 v2 t2
>   let ttup = TTuple v1 v2
>   unify p v ttup
>   return (ttup, newinsts2)
> unifyWith insts v (GramListType p t) = do
>   vel <- fresh
>   (vel, newinsts) <- unifyWith insts vel t
>   let tlist = TList vel
>   unify p v tlist
>   return (tlist, newinsts)
> unifyWith insts v t = do
>   unify (getTypePos t) v (convertFromGramType t)
>   v <- convert v
>   return (v, insts)

> convertGramType :: GramType -> Environment GramType
> convertGramType gt = do
>   t <- convert $ convertFromGramType gt
>   return $ convertToGramType t

> convertFromGramType :: GramType -> Type
> convertFromGramType (GramBasicType _ BoolType) = TBool
> convertFromGramType (GramBasicType _ CharType) = TChar
> convertFromGramType (GramBasicType _ IntType)  = TInt
> convertFromGramType (GramTupleType _ t1 t2)    = TTuple (convertFromGramType t1) (convertFromGramType t2)
> convertFromGramType (GramListType _ t)         = TList (convertFromGramType t)
> convertFromGramType (GramIdType (Id _ i)) -- quits ungracefully; TODO wrap in Environment
>   | (not $ null i) && head i == 't' =
>     case readMaybe (tail i) :: Maybe Int of
>       Just vid -> TVar vid
>   | (not $ null i) && head i == 'v' =
>     case readMaybe (tail i) :: Maybe Int of
>       Just vid -> TFree vid

> convertToGramType :: Type -> GramType -- HOF or function var.s not supported
> convertToGramType TBool          = GramBasicType nP BoolType
> convertToGramType TChar          = GramBasicType nP CharType
> convertToGramType TInt           = GramBasicType nP IntType
> convertToGramType (TTuple t1 t2) = GramTupleType nP (convertToGramType t1) (convertToGramType t2)
> convertToGramType (TList t)      = GramListType nP (convertToGramType t)
> convertToGramType (TVar i)       = GramIdType (Id nP ("t" ++ (show i)))
> convertToGramType (TFree i)      = GramIdType (Id nP ("v" ++ (show i)))

> getTypePos :: GramType -> SourcePos
> getTypePos (GramBasicType p _) = p
> getTypePos (GramTupleType p _ _) = p
> getTypePos (GramListType p _) = p
> getTypePos (GramIdType (Id p _)) = p


===============================================================================
 Error handlers
===============================================================================

> addErrorDesc :: String -> Environment t -> Environment t
> addErrorDesc s = mapStateT (wrapError s)
>   where wrapError _ (Right r) = Right r
>         wrapError s (Left (msg,p)) = Left (s ++ msg, p)

> replaceErrorDesc :: String -> String -> Environment t -> Environment t
> replaceErrorDesc frm to = mapStateT (wrapError frm to)
>   where wrapError _ _ (Right r) = Right r
>         wrapError frm to (Left (msg,p))
>           | msg == frm = Left (to,  p)
>           | otherwise  = Left (msg, p)


===============================================================================
 Initialisation functions
===============================================================================

> initEnv :: EnvType
> initEnv = ([(-4, tempty), (-2, tprint)],[[("print", -2), ("isEmpty", -4)]],0)
>   where tprint = TScheme [TFree (-1)] TVoid
>         tempty = TScheme [TList (TFree (-3))] TBool

> nP :: SourcePos
> nP = newPos "<internal>" 0 0

