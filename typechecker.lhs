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
> unify TInt TInt   _  = Right []
> unify TBool TBool _  = Right []
> unify (TVar i) t p
>   | t == (TVar i)    = Right []
>   | occurs i t       = Left ("Can't unify1", p)
>   | otherwise        = Right [(i, t)]
> unify t (TVar i) p
>   | t == (TVar i)    = Right []
>   | occurs i t       = Left ("Can't unify2", p)
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
> unify t1 t2 p = Left ("Can't unify types " ++ show t1 ++ " " ++ show t2, p)



Type inference algorithm M

Starts with an environment, an AST and a suggested type (which may be a type variable).
Returns an Environment if a possible set of type variable substitutions has been found,
binding TVar 0 to the whole expression's type, or returns Nothing if none could be found.

to test something's type, call with and check binding for TVar 0:
parse = \x -> parseSPL $ lexer nP x
let Right [GramDeclFun (GramFuncDecl _ exin)] = parse ex
inferBlaT ([],[[]],1) exin (TVar 0)

To implement:
- functions (see slides for their M-algorithm version)

These require changes to the Environment structure; either additional items in the tuple,
or a different structure for Substitutions.

The below methods have been tested on small examples input as ASTs, not code -> type checking yet.


  let sub = unique ((envSubs env1) ++ (envSubs env2))



> inferDeclT :: Environment -> GramDecl -> Either TypeError Environment
> inferDeclT env (GramDeclVar varDecl) = inferVarDeclT env varDecl
> inferDeclT env (GramDeclFun (GramFuncDecl fid funcDeclTail)) = do
>   let funcvar = nextVar env
>   env1 <- declareVar env fid
>   let rettyp = fresh env1
>   let env2 = pushScope (inc env1)
>   env3 <- loadFunDeclArgs env2 funcDeclTail
>   let argtypes = sort [TVar (snd arg) | arg <- head (envScopes env3)]
>   let funcsub = [(funcvar, TFunc argtypes rettyp)]
>   let sub = concatSubsts (addSubsts (envSubs env2) env3) funcsub
>   env4 <- inferFunDeclT env3 fid funcDeclTail (applySub sub rettyp)
>   let sub2 = concatSubsts (envSubs env4) sub
>   return $ popScope (sub2, envScopes env4, nextVar env4)


> inferFunDeclT :: Environment -> GramId -> GramFuncDeclTail -> Type -> Either TypeError Environment
> inferFunDeclT env fid (GramFuncDeclTail _ _ stmts) rettyp = do
>   env1 <- inferBlockT (pushScope env) stmts rettyp
>   return $ popScope env1

   let argtypes = map (applySub (envSubs env1)) $ sort [TVar (snd arg) | arg <- head (scopes env1)]
   let functyp = TFunc argtypes (applySub (envSubs env1) rettyp)
   let funcvar = nextVar env1
   env2 <- declareVar (popScope env1) fid
   return (addSubsts [(funcvar, functyp)] env2, envScopes env2, nextVar env2)

> checkNumArgs :: [GramFArgs] -> [GramFTypes] -> Bool
> checkNumArgs [] [] = True
> checkNumArgs [GramFArgsId _ fargs] [GramFTypes _ ftypes] = checkNumArgs fargs ftypes
> checkNumArgs _ _ = False

> loadFunDeclArgs :: Environment -> GramFuncDeclTail -> Either TypeError Environment
> loadFunDeclArgs env (GramFuncDeclTail [] [] _) = Right env
> loadFunDeclArgs env (GramFuncDeclTail [] [GramFunType [] _] _) = Right env
> loadFunDeclArgs env (GramFuncDeclTail [GramFArgsId (Id p vid) fargs] [] rettyp) = do
>   env1 <- declareVar env (Id p vid)
>   loadFunDeclArgs env1 (GramFuncDeclTail fargs [] rettyp)
> loadFunDeclArgs env (GramFuncDeclTail [GramFArgsId (Id p vid) fargs] [GramFunType [GramFTypes gt ftypes] r1] r2) 
>   | not (checkNumArgs fargs ftypes) = Left ("Number of arguments doesnt match.", p)
>   | otherwise = do
>       let t = convertType gt
>       env1 <- declareVar env (Id p vid)
>       var  <- varType env1 (Id p vid)
>       sub1 <- unify var t p
>       let env2 = (addSubsts sub1 env1, envScopes env1, nextVar env1)
>       loadFunDeclArgs env2 (GramFuncDeclTail fargs [GramFunType ftypes r1] r2)
> loadFunDeclArgs _ _ = Left("Error when loading function declaration args", newPos "test_file" 1 1)

> inferVarDeclT :: Environment -> GramVarDecl -> Either TypeError Environment
> inferVarDeclT env (GramVarDeclVar (GramVarDeclTail vid e)) = do
>   env1 <- declareVar env vid
>   var  <- varType env1 vid
>   inferExpT env1 e var
> inferVarDeclT env (GramVarDeclType gt (GramVarDeclTail (Id p i) e)) = do
>   let t = convertType gt
>   env1 <- declareVar env (Id p i)
>   var  <- varType env1 (Id p i)
>   sub1 <- unify var t p
>   let env2 = (addSubsts sub1 env1, envScopes env1, nextVar env1)
>   inferExpT env2 e (applySub (envSubs env2) t)



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
>   return (unique sub, envScopes (popScope env3), nextVar env3)
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
> inferStmtT env (GramStmtFunCall (GramFunCall (Id p _) _)) rettyp = Left ("Error on infetStmt", p)
> inferStmtT env (GramFunVarDecl vardecl) rettyp     = inferVarDeclT env vardecl
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
> inferExpT env (GramBinary p LessThan e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p LessOrEqual e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p GreaterThan e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt p
> inferExpT env (GramBinary p GreatherOrEqual e1 e2) t = inferBinExprT env e1 e2 t TInt TInt p
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
> inferExpT env (GramExpFunCall (GramFunCall (Id p _) _)) t   = Left ("Cant infer", p)
> inferExpT env (GramExpTuple p  e1 e2) t                     = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   let fresh2 = fresh env1
>   env2 <- inferExpT (inc env1) e2 fresh2
>   let sub = addSubsts (envSubs env2) env1
>   res  <- unify (applySub sub t) (applySub sub (TTuple fresh1 fresh2)) p
>   return (concatSubsts sub res, envScopes env2, nextVar env2)

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
>     Just subbed -> resolveSubsts envSubs ((i, subbed) : subs)
>     Nothing     -> (i, TVar j) : (resolveSubsts envSubs subs)
> resolveSubsts envSubs (sub : subs) = sub : (resolveSubsts envSubs subs)

> concatSubsts :: Substitutions -> Substitutions -> Substitutions
> concatSubsts old new = resolveSubsts concat $ unique $ map sub concat
>   where concat = old ++ new
>         sub = \x -> (fst x, applySub concat (snd x))

> addSubsts :: Substitutions -> Environment -> Substitutions
> addSubsts subs (envSubs, s, i) = concatSubsts envSubs subs













