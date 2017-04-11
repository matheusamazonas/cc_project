> import Data.Set (toList, fromList)
> import Grammar
> import Token 

> data Type = TBool | TInt | TChar | TVoid | TTuple Type Type | TList Type | TFunc [Type] Type | TVar Int
>   deriving (Show, Eq, Ord)

A substitution is a function from type variables (numbered by Ints) to Types.
A Substitutions is a list of these and an Environment is a list of them,
accompanied by the number of the next fresh type variable.

> type Substitutions = [(Int, Type)]
> type Scope = [(GramId, Int)]
> type Environment = (Substitutions, [Scope], Int)



Unification algorithm U

Returns Just a list of substitutions ((TVar) Int -> Type) if the two types
can be unified, otherwise returns Nothing

> unify :: Type -> Type -> Maybe Substitutions
> unify TInt TInt    = Just []
> unify TBool TBool  = Just []
> unify (TVar i) t
>   | t == (TVar i)  = Just []
>   | occurs i t     = Nothing
>   | otherwise      = Just [(i, t)]
> unify t (TVar i) 
>   | t == (TVar i)  = Just []
>   | occurs i t     = Nothing
>   | otherwise      = Just [(i, t)]
> unify (TList ta) (TList tb) = unify ta tb
> unify (TTuple ta1 ta2) (TTuple tb1 tb2) = do
>   sub1 <- unify ta1 tb1
>   sub2 <- unify (applySub sub1 ta2) (applySub sub1 tb2)
>   return (unique (sub1 ++ sub2))
> unify (TFunc [] ta2) (TFunc [] tb2) = unify ta2 tb2
> unify (TFunc (ta1:ta1s) ta2) (TFunc (tb1:tb1s) tb2) = do
>   sub1 <- unify ta1 tb1
>   let nta1s = [applySub sub1 nta1 | nta1 <- ta1s]
>   let ntb1s = [applySub sub1 ntb1 | ntb1 <- tb1s]
>   sub2 <- unify (TFunc nta1s ta2) (TFunc ntb1s tb2)
>   return (unique (sub1 ++ sub2))
> unify _ _          = Nothing


Type inference algorithm M

Starts with an environment, an AST and a suggested type (which may be a type variable).
Returns an Environment if a possible set of type variable substitutions has been found,
binding TVar 0 to the whole expression's type, or returns Nothing if none could be found.

to test something's type, call with and check binding for TVar 0:
inferBlaT ([],[[]],1) (<expression>) (TVar 0)

To implement:
- statements (if, while) when also type-checking statements
- variables and their fields
- functions (see slides for their M-algorithm version)

These require changes to the Environment structure; either additional items in the tuple,
or a different structure for Substitutions.

The below methods have been tested on small examples input as ASTs, not code -> type checking yet.






> inferDeclT :: Environment -> GramDecl -> Maybe Environment
> inferDeclT env (GramDeclFun (GramFuncDecl id funcDeclTail)) = Nothing
> inferDeclT env (GramDeclVar varDecl) = inferVarDeclT env varDecl

 inferFunDeclT :: Environment -> GramId -> GramFuncDeclTail -> Type -> Maybe Environment

> inferVarDeclT :: Environment -> GramVarDecl -> Maybe Environment
> inferVarDeclT env (GramVarDeclVar (GramVarDeclTail vid e)) = do
>   env1 <- declareVar env vid
>   var  <- varType env1 vid
>   inferExpT env1 e var
> inferVarDeclT env (GramVarDeclType gt (GramVarDeclTail vid e)) = do
>   let t = convertType gt
>   env1 <- declareVar env vid
>   var  <- varType env1 vid
>   sub1 <- unify var t
>   let env2 = (unique (envSubs env1 ++ sub1), envScopes env1, nextVar env1)
>   inferExpT env2 e t



> inferBlockT :: Environment -> [GramStmt] -> Type -> Maybe Environment
> inferBlockT env [] _ = Just env
> inferBlockT env (stmt:stmts) t = do
>   env1 <- inferStmtT env stmt t
>   inferBlockT env1 stmts t

> inferStmtT :: Environment -> GramStmt -> Type -> Maybe Environment
> inferStmtT env (GramIf p cond tr fa) rettyp        = do
>   env1 <- inferExpT env cond TBool
>   env2 <- inferBlockT (pushScope env1) tr (applySub (envSubs env1) rettyp)
>   env3 <- inferBlockT (pushScope (popScope env2)) fa (applySub ((envSubs env1) ++ (envSubs env2)) rettyp)
>   let sub = (envSubs env1) ++ (envSubs env2) ++ (envSubs env3)
>   return (unique sub, envScopes (popScope env3), nextVar env3)
> inferStmtT env (GramWhile p cond loop) rettyp      = do
>   env1 <- inferExpT env cond TBool
>   env2 <- inferBlockT (pushScope env1) loop (applySub (envSubs env1) rettyp)
>   let sub = (envSubs env1) ++ (envSubs env2)
>   return (unique sub, envScopes (popScope env2), nextVar env2)
> inferStmtT env (GramAttr p var exp) rettyp         = do
>   let fresh1 = fresh env
>   env1 <- inferVarT (inc env) var fresh1
>   env2 <- inferExpT env1 exp (applySub (envSubs env1) fresh1)
>   return (unique ((envSubs env1) ++ (envSubs env2)), envScopes env2, nextVar env2)
> inferStmtT env (GramStmtFunCall _) rettyp          = Nothing
> inferStmtT env (GramFunVarDecl vardecl) rettyp     = inferVarDeclT env vardecl
> inferStmtT env (GramReturn p ret) rettyp           = 
>   case ret of
>     Just exp -> inferExpT env exp rettyp
>     Nothing  -> 
>       case unify TVoid rettyp of
>         Just sub -> Just (unique ((envSubs env) ++ sub), envScopes env, nextVar env)
>         Nothing  -> Nothing
> inferstmtT _ _                                     = Nothing


> inferExpT :: Environment -> GramExp -> Type -> Maybe Environment
> inferExpT env (GramBool p _) t                       = liftMaybe (envSubs env +?+ unify TBool t, envScopes env, nextVar env)
> inferExpT env (GramChar p _) t                       = liftMaybe (envSubs env +?+ unify TChar t, envScopes env, nextVar env)
> inferExpT env (GramNum p _) t                        = liftMaybe (envSubs env +?+ unify TInt t, envScopes env, nextVar env)
> inferExpT env (GramEmptyList p) t                    = liftMaybe (envSubs env +?+ unify (TList (fresh env)) t, envScopes env, nextVar env + 1)
> inferExpT env (GramBinary p Minus e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p Plus e1 e2) t            = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p Times e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p Division e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p Mod e1 e2) t             = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p LessThan e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p LessOrEqual e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p GreaterThan e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p GreatherOrEqual e1 e2) t = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary p LogicalOr e1 e2) t       = inferBinExprT env e1 e2 t TBool TBool
> inferExpT env (GramBinary p LogicalAnd e1 e2) t      = inferBinExprT env e1 e2 t TBool TBool
> inferExpT env (GramBinary p Equals e1 e2) t          = inferBinExprT (inc env) e1 e2 t (fresh env) TBool
> inferExpT env (GramBinary p Different e1 e2) t       = inferBinExprT (inc env) e1 e2 t (fresh env) TBool
> inferExpT env (GramBinary p ListConst e1 e2) t       = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   env2 <- inferExpT env1 e2 (applySub (envSubs env1) (TList fresh1))
>   let sub = (envSubs env1) ++ (envSubs env2)
>   res <- unify (applySub sub t) (applySub sub (TList fresh1))
>   return (unique (sub ++ res), envScopes env2, nextVar env2)
> inferExpT env (GramUnary p Minus e) t                = inferUnExprT env e t TInt
> inferExpT env (GramUnary p LogicalNot e) t           = inferUnExprT env e t TBool
> inferExpT env (GramExpId gv) t                       = inferVarT env gv t
> inferExpT env (GramExpFunCall (GramFunCall i _)) t   = Nothing
> inferExpT env (GramExpTuple p  e1 e2) t              = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   let fresh2 = fresh env1
>   env2 <- inferExpT (inc env1) e2 fresh2
>   let sub = (envSubs env1) ++ (envSubs env2)
>   res  <- unify (applySub sub t) (applySub sub (TTuple fresh1 fresh2))
>   return (unique (sub ++ res), envScopes env2, nextVar env2)

> inferVarT :: Environment -> GramVar -> Type -> Maybe Environment
> inferVarT env (Var vid gf) t = do
>   typ <- varType env vid
>   inferFieldT env typ gf t

> inferFieldT :: Environment -> Type -> [GramField] -> Type -> Maybe Environment
> inferFieldT env typ [] t = do
>   sub <- unify typ t
>   return (unique (envSubs env ++ sub), envScopes env, nextVar env)
> inferFieldT env (TTuple typa _) [(First p gf)] t = inferFieldT env typa gf t
> inferFieldT env (TTuple _ typb) [(Second p gf)] t = inferFieldT env typb gf t
> inferFieldT env (TList typ) [(Head p gf)] t = inferFieldT env typ gf t
> inferFieldT env (TList typ) [(Tail p gf)] t = inferFieldT env (TList typ) gf t


> inferBinExprT :: Environment -> GramExp -> GramExp -> Type -> Type -> Type -> Maybe Environment
> inferBinExprT env e1 e2 texp telem tres = do
>   env1 <- inferExpT env e1 telem
>   env2 <- inferExpT env1 e2 (applySub (envSubs env1) telem)
>   let sub = (envSubs env1) ++ (envSubs env2)
>   res  <- unify (applySub sub texp) (applySub sub tres)
>   return (unique (sub ++ res), envScopes env2, nextVar env2)

> inferUnExprT :: Environment -> GramExp -> Type -> Type -> Maybe Environment
> inferUnExprT env e texp tcomp = do
>   env1 <- inferExpT env e tcomp
>   let sub = envSubs env1
>   res <- unify (applySub sub texp) (applySub sub tcomp)
>   return (unique (sub ++ res), envScopes env1, nextVar env1)


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

> varType :: Environment -> GramId -> Maybe Type
> varType (subs, scopes, nxt) vid =
>   case varId (subs, scopes, nxt) vid of
>     Nothing -> Nothing
>     Just i  ->
>       case lookup i subs of
>         Nothing -> Just (TVar i)
>         Just t  -> Just t

> varId :: Environment -> GramId -> Maybe Int
> varId (_, [], _) _ = Nothing
> varId (subs, scope:scopes, nxt) vid =
>   case lookup vid scope of
>     Just i  -> Just i
>     Nothing -> varId (subs, scopes, nxt) vid

> declareVar :: Environment -> GramId -> Maybe Environment
> declareVar (subs, scope:scopes, nxt) vid =
>   case lookup vid scope of
>     Just _  -> Nothing
>     Nothing -> Just (subs, ((vid, nxt):scope):scopes, nxt+1)

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

> (+?+) :: Substitutions -> Maybe Substitutions -> Maybe Substitutions
> (+?+) suba (Just subb) = Just (suba ++ subb)
> (+?+) suba Nothing     = Nothing

> liftMaybe :: (Maybe Substitutions, [Scope],Int) -> Maybe Environment
> liftMaybe (Nothing, _, _)        = Nothing
> liftMaybe (Just sub, scopes, i)  = Just (sub, scopes, i)

> applySub :: Substitutions -> Type -> Type
> applySub [] t                    = t
> applySub sub (TTuple ta tb)      = TTuple (applySub sub ta) (applySub sub tb)
> applySub sub (TList t)           = TList (applySub sub t)
> applySub sub (TFunc tas tb)      = TFunc [applySub sub ta | ta <- tas] (applySub sub tb)
> applySub sub (TVar i)            = case lookup i sub of Just subbed -> subbed
>                                                         Nothing     -> (TVar i)
> applySub _ t                     = t