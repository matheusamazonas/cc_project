> module TypeChecker where

> import Data.Set (toList, fromList)
> import Data.List (sort)
> import Grammar
> import Text.Parsec.Pos (newPos, SourcePos)
> import Token 

> import Lexer

> data Type = TBool | TInt | TChar | TVoid | TTuple Type Type | TList Type | TFunc [Type] Type | TVar Int
>   deriving (Show, Eq, Ord)

A substitution is a function from type variables (numbered by Ints) to Types.
A Substitutions is a list of these and an Environment is a list of them,
accompanied by the number of the next fresh type variable.

> type Substitutions = [(Int, Type)]
> type Scope = [(String, Int)]
> type Environment = (Substitutions, [Scope], Int)
> type TypeError = (String, SourcePos)


Unification algorithm U

Returns Just a list of substitutions ((TVar) Int -> Type) if the two types
can be unified, otherwise returns Nothing

> unify :: Type -> Type -> SourcePos -> Either TypeError Substitutions
> unify TVoid TVoid _  = Right []
> unify TChar TChar _  = Right []
> unify TInt TInt _    = Right []
> unify TBool TBool _  = Right []
> unify (TVar i) t p
>   | t == (TVar i)    = Right []
>   | occurs i t       = Left ("Recursive type detected", p)
>   | otherwise        = Right [(i, t)]
> unify t (TVar i) p
>   | t == (TVar i)    = Right []
>   | occurs i t       = Left ("Recursive type detected", p)
>   | otherwise        = Right [(i, t)]
> unify (TList ta) (TList tb) p = unify ta tb p
> unify (TTuple ta1 ta2) (TTuple tb1 tb2) p = do
>   sub1 <- unify ta1 tb1 p
>   sub2 <- unify (applySub sub1 ta2) (applySub sub1 tb2) p
>   return $ concatSubsts sub1 sub2
> unify (TFunc [] ta2) (TFunc [] tb2) p = unify ta2 tb2 p
> unify (TFunc (ta1:ta1s) ta2) (TFunc (tb1:tb1s) tb2) p = do
>   sub1 <- unify ta1 tb1 p
>   let nta1s = [applySub sub1 nta1 | nta1 <- ta1s]
>   let ntb1s = [applySub sub1 ntb1 | ntb1 <- tb1s]
>   sub2 <- unify (TFunc nta1s ta2) (TFunc ntb1s tb2) p
>   return $ concatSubsts sub1 sub2
> unify t1 t2 p = Left ("Can't unify types " ++ show t1 ++ " and " ++ show t2, p)



Type checking and inference

Test with:

parse = \x -> Test.parseSPL $ lexer (newPos "stdin" 1 1) x
let Right prog = parse "<program>"
inferGramT ([],[[]],1) prog


!! TODO !! inferDeclT give error when variable used as function (var x = 5; x())
this is detectable because it's not a TFunc


> inferProgT :: [GramDecl] -> Either TypeError Environment
> inferProgT prog = do
>   env <- initGramSignT ([],[[]],0) prog
>   inferGramT env prog

> inferGramT :: Environment -> [GramDecl] -> Either TypeError Environment
> inferGramT env [] = Right env
> inferGramT env (decl:decls) = do
>   env1 <- inferDeclT env decl
>   inferGramT env1 decls


> inferDeclT :: Environment -> GramDecl -> Either TypeError Environment
> inferDeclT env (GramDeclVar varDecl) = inferVarDeclT env varDecl
> inferDeclT env (GramDeclFun (GramFuncDecl (Id p fid) funcDeclTail)) = do
>   let Just funcid = varId env fid
>   loaded <- loadFunDeclArgs (pushScope (envSubs env, envScopes env, funcid+1)) funcDeclTail
>   let rettyp = TVar (nextVar loaded)
>   let env1 = (envSubs env, envScopes loaded, nextVar env)
>   env2 <- inferFunDeclT env1 (Id p fid) funcDeclTail (applySub (envSubs env1) rettyp)
>   let sub = concatSubsts (envSubs env2) (envSubs env1)
>   return $ popScope (sub, envScopes env2, nextVar env2)

Old version assuming empty environment:
  env1 <- initFunDeclSignT env fid funcDeclTail
  let rettyp = TVar (nextVar env1 - 1)
  env2 <- inferFunDeclT env1 fid funcDeclTail (applySub (envSubs env1) rettyp)
  let sub = concatSubsts (envSubs env2) (envSubs env1)
  return $ popScope (sub, envScopes env2, nextVar env2)

> initGramSignT :: Environment -> [GramDecl] -> Either TypeError Environment
> initGramSignT env [] = Right env
> initGramSignT env (decl:decls) = do
>   env1 <- initDeclSignT env decl
>   initGramSignT env1 decls

> initDeclSignT :: Environment -> GramDecl -> Either TypeError Environment
> initDeclSignT env (GramDeclVar (GramVarDeclVar (GramVarDeclTail vid e))) = declareVar env vid
> initDeclSignT env (GramDeclVar (GramVarDeclType gt (GramVarDeclTail vid e))) = do
>   let varvar = nextVar env
>   env1 <- declareVar env vid
>   let varsub = [(varvar, convertType gt)]
>   let sub = addSubsts varsub env1
>   return (sub, envScopes env1, nextVar env1)
> initDeclSignT env (GramDeclFun (GramFuncDecl fid funcDeclTail)) = do
>   env1 <- initFunDeclSignT env fid funcDeclTail
>   return $ popScope env1

> initFunDeclSignT :: Environment -> GramId -> GramFuncDeclTail -> Either TypeError Environment
> initFunDeclSignT env fid (GramFuncDeclTail fargs ftypes stmts) = do
>   let funcvar = nextVar env
>   env1 <- declareVar env fid
>   let env2 = pushScope env1
>   env3 <- inferFunDeclArgs env2 (GramFuncDeclTail fargs ftypes stmts)
>   let argtypes = sort [TVar (snd arg) | arg <- head (envScopes env3)]
>   let rettyp = TVar (nextVar env3 - 1)
>   let funcsub = [(funcvar, TFunc argtypes rettyp)]
>   let sub = concatSubsts (addSubsts (envSubs env2) env3) funcsub
>   return (sub, envScopes env3, nextVar env3)

> inferFunDeclT :: Environment -> GramId -> GramFuncDeclTail -> Type -> Either TypeError Environment
> inferFunDeclT env fid (GramFuncDeclTail _ _ stmts) rettyp = do
>   env1 <- inferBlockT (pushScope env) stmts rettyp
>   return $ popScope env1

> loadFunDeclArgs :: Environment -> GramFuncDeclTail -> Either TypeError Environment
> loadFunDeclArgs env (GramFuncDeclTail [] _ _) = Right env
> loadFunDeclArgs env (GramFuncDeclTail [GramFArgsId (Id p vid) fargs] _ rettyp) = do
>   let scope:scopes = envScopes env
>   let arg = (vid, nextVar env)
>   let env1 = (envSubs env, ((arg:scope):scopes), nextVar env + 1)
>   loadFunDeclArgs env1 (GramFuncDeclTail fargs [] rettyp)
> loadFunDeclArgs _ _ = Left("Error when loading function declaration args", newPos "test_file" 1 1)


> checkNumArgs :: [GramFArgs] -> [GramFTypes] -> Bool
> checkNumArgs [] [] = True
> checkNumArgs [GramFArgsId _ fargs] [GramFTypes _ ftypes] = checkNumArgs fargs ftypes
> checkNumArgs _ _ = False

> inferFunDeclArgs :: Environment -> GramFuncDeclTail -> Either TypeError Environment
> inferFunDeclArgs env (GramFuncDeclTail [] [] _) = Right (inc env)
> inferFunDeclArgs env (GramFuncDeclTail [] [GramFunType [] rettyp] _) = Right (inc env1)
>   where retType (GramRetType t) = convertType t
>         retType (GramVoidType _) = TVoid
>         env1 = (concatSubsts (envSubs env) [(nextVar env, retType rettyp)], envScopes env, nextVar env)
> inferFunDeclArgs env (GramFuncDeclTail [GramFArgsId (Id p vid) fargs] [] rettyp) = do
>   env1 <- declareVar env (Id p vid)
>   inferFunDeclArgs env1 (GramFuncDeclTail fargs [] rettyp)
> inferFunDeclArgs env (GramFuncDeclTail [GramFArgsId (Id p vid) fargs] [GramFunType [GramFTypes gt ftypes] r1] r2) 
>   | not (checkNumArgs fargs ftypes) = Left ("Number of arguments doesnt match.", p)
>   | otherwise = do
>       let t = convertType gt
>       env1 <- declareVar env (Id p vid)
>       var  <- varType env1 (Id p vid)
>       sub1 <- unify var t p
>       let env2 = (addSubsts sub1 env1, envScopes env1, nextVar env1)
>       inferFunDeclArgs env2 (GramFuncDeclTail fargs [GramFunType ftypes r1] r2)
> inferFunDeclArgs _ _ = Left("Error when loading function declaration args", newPos "test_file" 1 1)

inferVarDeclT without loading from template used:
  env1 <- declareVar env vid

> inferVarDeclT :: Environment -> GramVarDecl -> Either TypeError Environment
> inferVarDeclT env (GramVarDeclVar (GramVarDeclTail vid e)) = do
>   var  <- varType env vid
>   inferExpT env e var
> inferVarDeclT env (GramVarDeclType gt (GramVarDeclTail (Id p i) e)) = do
>   let t = convertType gt
>   var  <- varType env (Id p i)
>   sub <- unify var t p
>   let env1 = (addSubsts sub env, envScopes env, nextVar env)
>   inferExpT env1 e (applySub (envSubs env1) t)



> inferBlockT :: Environment -> [GramStmt] -> Type -> Either TypeError Environment
> inferBlockT env [] _ = Right env
> inferBlockT env (stmt:stmts) t = do
>   env1 <- inferStmtT env stmt t
>   inferBlockT env1 stmts (applySub (envSubs env1) t)

> inferStmtT :: Environment -> GramStmt -> Type -> Either TypeError Environment
> inferStmtT env (GramIf p cond tr fa) rettyp        = do
>   env1 <- inferExpT env cond TBool
>   env2 <- inferBlockT (pushScope env1) tr (applySub (envSubs env1) rettyp)
>   env3 <- inferBlockT (pushScope (popScope env2)) fa (applySub (addSubsts (envSubs env1) env2) rettyp)
>   let sub = addSubsts (addSubsts (envSubs env1) env2) env3
>   return (sub, envScopes (popScope env3), nextVar env3)
> inferStmtT env (GramWhile p cond loop) rettyp      = do
>   env1 <- inferExpT env cond TBool
>   env2 <- inferBlockT (pushScope env1) loop (applySub (envSubs env1) rettyp)
>   let sub = addSubsts (envSubs env2) env1
>   return (unique sub, envScopes (popScope env2), nextVar env2)
> inferStmtT env (GramAttr p var exp) rettyp         = do
>   let fresh1 = fresh env
>   env1 <- inferVarT (inc env) var fresh1
>   env2 <- inferExpT env1 exp (applySub (envSubs env1) fresh1)
>   return (addSubsts (envSubs env2) env1, envScopes env2, nextVar env2)
> inferStmtT env (GramFunVarDecl vardecl) rettyp     = do
>   env1 <- initDeclSignT env (GramDeclVar vardecl)
>   inferVarDeclT env1 vardecl
> inferStmtT env (GramStmtFunCall (GramFunCall (Id p i) args)) rettyp = do
>   TFunc a1 r1 <- varType env (Id p i)
>   if (length a1 /= argListLength args)
>     then Left ("Wrong number of arguments.", p)
>     else do
>       env1 <- inferArgListType env args a1
>       return env1
> inferStmtT env (GramReturn p ret) rettyp           = 
>   case ret of
>     Just exp -> inferExpT env exp rettyp
>     Nothing  -> 
>       case unify TVoid rettyp p of
>         Right sub   -> Right (addSubsts sub env, envScopes env, nextVar env)
>         Left uError -> Left uError
> inferstmtT _ _                                     = Left ("Error on infetStmt", newPos "test_file" 1 1)



> inferExpT :: Environment -> GramExp -> Type -> Either TypeError Environment
> inferExpT env (GramBool p _) t                       = liftMaybe (envSubs env +?+ unify TBool t p, envScopes env, nextVar env) p
> inferExpT env (GramChar p _) t                       = liftMaybe (envSubs env +?+ unify TChar t p, envScopes env, nextVar env) p
> inferExpT env (GramNum p _) t                        = liftMaybe (envSubs env +?+ unify TInt t p, envScopes env, nextVar env) p
> inferExpT env (GramEmptyList p) t                    = liftMaybe (envSubs env +?+ unify (TList (fresh env)) t p, envScopes env, nextVar env + 1) p
> inferExpT env (GramBinary p Minus e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p Plus e1 e2) t            = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p Times e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p Division e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p Mod e1 e2) t             = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p LessThan e1 e2) t        = inferBinExprT env e1 e2 t TInt TBool p
> inferExpT env (GramBinary p LessOrEqual e1 e2) t     = inferBinExprT env e1 e2 t TInt TBool p
> inferExpT env (GramBinary p GreaterThan e1 e2) t     = inferBinExprT env e1 e2 t TInt TBool p
> inferExpT env (GramBinary p GreatherOrEqual e1 e2) t = inferBinExprT env e1 e2 t TInt TBool p
> inferExpT env (GramBinary p LogicalOr e1 e2) t       = inferBinExprT env e1 e2 t TBool TBool p
> inferExpT env (GramBinary p LogicalAnd e1 e2) t      = inferBinExprT env e1 e2 t TBool TBool p
> inferExpT env (GramBinary p Equals e1 e2) t          = inferBinExprT (inc env) e1 e2 t (fresh env) TBool p
> inferExpT env (GramBinary p Different e1 e2) t       = inferBinExprT (inc env) e1 e2 t (fresh env) TBool p
> inferExpT env (GramBinary p ListConst e1 e2) t       = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   env2 <- inferExpT env1 e2 (applySub (envSubs env1) (TList fresh1))
>   let sub = addSubsts (envSubs env2) env1
>   res <- unify (applySub sub t) (applySub sub (TList fresh1)) p
>   return (concatSubsts sub res, envScopes env2, nextVar env2)
> inferExpT env (GramUnary p Minus e) t                       = inferUnExprT env e t TInt p
> inferExpT env (GramUnary p LogicalNot e) t                  = inferUnExprT env e t TBool p
> inferExpT env (GramExpId gv) t                              = inferVarT env gv t
> inferExpT env (GramExpFunCall (GramFunCall (Id p i) args)) t   = do
>   TFunc a1 r1 <- varType env (Id p i)
>   if (length a1 /= argListLength args)
>     then Left ("Wrong number of arguments.", p)
>     else do
>       env1 <- inferArgListType env args a1
>       subs <- unify t r1 p 
>       return (addSubsts subs env1, envScopes env, nextVar env)
> inferExpT env (GramExpTuple p  e1 e2) t                     = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   let fresh2 = fresh env1
>   env2 <- inferExpT (inc env1) e2 fresh2
>   let sub = addSubsts (envSubs env2) env1
>   res  <- unify (applySub sub t) (applySub sub (TTuple fresh1 fresh2)) p
>   return (concatSubsts sub res, envScopes env2, nextVar env2)

GramArgList [GramActArgs]
GramActExpr GramExp [GramActArgs]

> argListLength :: GramArgList -> Int
> argListLength (GramArgList []) = 0
> argListLength (GramArgList ((GramActExpr _ args):lst)) = 1 + argListLength (GramArgList args)

> inferVarT :: Environment -> GramVar -> Type -> Either TypeError Environment
> inferVarT env (Var (Id p i) gf) t = do
>   typ <- varType env (Id p i)
>   inferFieldT env typ gf t p


> inferFieldT :: Environment -> Type -> [GramField] -> Type -> SourcePos -> Either TypeError Environment
> inferFieldT env typ [] t sp = do
>   sub <- unify typ t sp
>   return (addSubsts sub env, envScopes env, nextVar env)
> inferFieldT env (TTuple typa _) [(First p gf)] t sp = inferFieldT env typa gf t sp
> inferFieldT env (TTuple _ typb) [(Second p gf)] t sp = inferFieldT env typb gf t sp
> inferFieldT env (TList typ) [(Head p gf)] t sp = inferFieldT env typ gf t sp
> inferFieldT env (TList typ) [(Tail p gf)] t sp = inferFieldT env (TList typ) gf t sp 


> inferBinExprT :: Environment -> GramExp -> GramExp -> Type -> Type -> Type -> SourcePos -> Either TypeError Environment
> inferBinExprT env e1 e2 texp telem tres p = do
>   env1 <- inferExpT env e1 telem
>   env2 <- inferExpT env1 e2 (applySub (envSubs env1) telem)
>   let sub = addSubsts (envSubs env2) env1
>   res  <- unify (applySub sub texp) (applySub sub tres) p
>   return (concatSubsts sub res, envScopes env2, nextVar env2)

> inferUnExprT :: Environment -> GramExp -> Type -> Type -> SourcePos -> Either TypeError Environment
> inferUnExprT env e texp tcomp p = do
>   env1 <- inferExpT env e tcomp
>   let sub = envSubs env1
>   res <- unify (applySub sub texp) (applySub sub tcomp) p
>   return (concatSubsts sub res, envScopes env1, nextVar env1)

> inferArgListType :: Environment -> GramArgList -> [Type] -> Either TypeError Environment
> inferArgListType env (GramArgList []) [] = return env
> inferArgListType env (GramArgList ((GramActExpr e es):_)) (t:ts) = do
>   env1 <- inferExpT env e t
>   inferArgListType env1 (GramArgList es) ts

N.B. convertType misses GramIdType GramId, which defines (unused and forbidden) custom types..

> convertType :: GramType -> Type
> convertType (GramBasicType p IntType) = TInt
> convertType (GramBasicType p BoolType) = TBool
> convertType (GramBasicType p CharType) = TChar
> convertType (GramTupleType p ta tb) = TTuple (convertType ta) (convertType tb)
> convertType (GramListType p t) = TList (convertType t)




Occurs check

> occurs :: Int -> Type -> Bool
> occurs i (TTuple ta tb)          = (occurs i ta) || (occurs i tb)
> occurs i (TList t)               = occurs i t
> occurs i (TFunc [] tb)           = (occurs i tb)
> occurs i (TFunc (ta1:ta1s) tb)   = (occurs i ta1) || (occurs i (TFunc ta1s tb))
> occurs i (TVar j)                = i == j
> occurs _ _ = False


Scope checks

> varType :: Environment -> GramId -> Either TypeError Type
> varType (subs, scopes, nxt) (Id p iD) =
>   case varId (subs, scopes, nxt) iD of
>     Nothing -> Left ("Use of undeclared variable '" ++ iD ++ "'", p)
>     Just i  ->
>       case lookup i subs of
>         Nothing -> Right (TVar i)
>         Just t  -> Right t

> varId :: Environment -> String -> Maybe Int
> varId (_, [], _) _ = Nothing
> varId (subs, scope:scopes, nxt) vid =
>   case lookup vid scope of
>     Just i  -> Just i
>     Nothing -> varId (subs, scopes, nxt) vid

> declareVar :: Environment -> GramId -> Either TypeError Environment
> declareVar (subs, scope:scopes, nxt) (Id p i) =
>   case lookup i scope of
>     Just _  -> Left ("Variable '" ++ i ++ "' was declared multiple times.", p)
>     Nothing -> Right (subs, ((i, nxt):scope):scopes, nxt+1)

> pushScope :: Environment -> Environment
> pushScope (subs, scopes, nxt) = (subs, []:scopes, nxt)

> popScope :: Environment -> Environment
> popScope (subs, scope:scopes, nxt) = (subs, scopes, nxt)


Substitution / environment manipulation

> envSubs :: Environment -> Substitutions
> envSubs (subs, _, _)             = subs

> envScopes :: Environment -> [Scope]
> envScopes (_, scopes, _)            = scopes

> nextVar :: Environment -> Int
> nextVar (_, _, nxt)              = nxt

> fresh :: Environment -> Type
> fresh env                        = TVar (nextVar env)

> inc :: Environment -> Environment
> inc (subs, scopes, i)      = (subs, scopes, i+1)

> unique :: (Ord a) => [a] -> [a]
> unique = toList . fromList

> (+?+) :: Substitutions -> Either TypeError Substitutions -> Either TypeError Substitutions
> (+?+) suba (Right subb) = Right (concatSubsts subb suba)
> (+?+) suba l    = l


> liftMaybe :: (Either TypeError Substitutions, [Scope], Int) -> SourcePos -> Either TypeError Environment
> liftMaybe (Left (e, p), _, _)    _ = Left (e,p)
> liftMaybe (Right sub, scopes, i) p = Right (sub, scopes, i)

> applySub :: Substitutions -> Type -> Type
> applySub [] t                    = t
> applySub sub (TTuple ta tb)      = TTuple (applySub sub ta) (applySub sub tb)
> applySub sub (TList t)           = TList (applySub sub t)
> applySub sub (TFunc tas tb)      = TFunc [applySub sub ta | ta <- tas] (applySub sub tb)
> applySub sub (TVar i)            = case lookup i sub of Just subbed -> subbed
>                                                         Nothing     -> (TVar i)
> applySub _ t                     = t

> resolveSubsts :: Substitutions -> Substitutions -> Substitutions
> resolveSubsts _ []                     = []
> resolveSubsts envSubs ((i, TVar j) : subs) = 
>   case lookup j envSubs of
>     Nothing     -> (i, TVar j) : (resolveSubsts envSubs subs)
>     Just subbed -> case () of
>       () | subbed == TVar i -> (i, TVar i) : (resolveSubsts envSubs subs)
>          | otherwise        -> resolveSubsts envSubs ((i, subbed) : subs)
> resolveSubsts envSubs (sub : subs) = sub : (resolveSubsts envSubs subs)

> concatSubsts :: Substitutions -> Substitutions -> Substitutions
> concatSubsts old new = resolveSubsts concat $ unique $ map sub concat
>   where concat = old ++ new
>         sub = \x -> (fst x, applySub concat (snd x))

> addSubsts :: Substitutions -> Environment -> Substitutions
> addSubsts subs (envSubs, s, i) = concatSubsts envSubs subs













