> import Grammar
> import Token 

> data Type = TBool | TInt | TChar | TVoid | TTuple Type Type | TList Type | TFunc [Type] Type | TVar Int
>   deriving (Show, Eq)

A substitution is a function from type variables (numbered by Ints) to Types.
A Substitutions is a list of these and an Environment is a list of them,
accompanied by the number of the next fresh type variable.

> type Substitutions = [(Int, Type)]
> type Environment = (Substitutions, Int)



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
>   return (sub1 ++ sub2)
> unify (TFunc [] ta2) (TFunc [] tb2) = unify ta2 tb2
> unify (TFunc (ta1:ta1s) ta2) (TFunc (tb1:tb1s) tb2) = do
>   sub1 <- unify ta1 tb1
>   let nta1s = [applySub sub1 nta1 | nta1 <- ta1s]
>   let ntb1s = [applySub sub1 ntb1 | ntb1 <- tb1s]
>   sub2 <- unify (TFunc nta1s ta2) (TFunc ntb1s tb2)
>   return (sub1 ++ sub2)
> unify _ _          = Nothing


Type inference algorithm M

Starts with an environment, an AST and a suggested type (which may be a type variable).
Returns an Environment if a possible set of type variable substitutions has been found,
binding TVar 0 to the whole expression's type, or returns Nothing if none could be found.

to test something's type, call with and check binding for TVar 0:
inferBlaT ([],1) (<expression>) (TVar 0)

To implement:
- statements (if, while) when also type-checking statements
- variables and their fields
- functions (see slides for their M-algorithm version)

These require changes to the Environment structure; either additional items in the tuple,
or a different structure for Substitutions.

The below methods have been tested on small examples input as ASTs, not code -> type checking yet.


> inferStmtT :: Environment -> GramStmt -> Type -> Maybe Environment
> inferStmtT env (GramIf cond tr fa) t               = Nothing
> inferStmtT env (GramWhile cond loop) t             = Nothing
> inferStmtT env (GramAttr var exp) t                = Nothing
> inferStmtT env (GramStmtFunCall _) t               = Nothing
> inferStmtT env (GramReturn ret) t                  = 
>   case ret of
>     Just exp -> inferExpT env exp t
>     Nothing  -> 
>       case unify TVoid t of
>         Just sub -> Just ((fst env) ++ sub, snd env)
>         Nothing  -> Nothing
> inferstmtT _ _                                     = Nothing


> inferExpT :: Environment -> GramExp -> Type -> Maybe Environment
> inferExpT env (GramBool _) t                       = liftMaybe (unify TBool t, snd env)
> inferExpT env (GramChar _) t                       = liftMaybe (unify TChar t, snd env)
> inferExpT env (GramNum _) t                        = liftMaybe (unify TInt t, snd env)
> inferExpT env (GramEmptyList) t                    = liftMaybe (unify (TList (fresh env)) t, snd env + 1)
> inferExpT env (GramBinary Minus e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary Plus e1 e2) t            = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary Times e1 e2) t           = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary Division e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary Mod e1 e2) t             = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary LessThan e1 e2) t        = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary LessOrEqual e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary GreaterThan e1 e2) t     = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary GreatherOrEqual e1 e2) t = inferBinExprT env e1 e2 t TInt TInt
> inferExpT env (GramBinary LogicalOr e1 e2) t       = inferBinExprT env e1 e2 t TBool TBool
> inferExpT env (GramBinary LogicalAnd e1 e2) t      = inferBinExprT env e1 e2 t TBool TBool
> inferExpT env (GramBinary Equals e1 e2) t          = inferBinExprT (inc env) e1 e2 t (fresh env) TBool
> inferExpT env (GramBinary Different e1 e2) t       = inferBinExprT (inc env) e1 e2 t (fresh env) TBool
> inferExpT env (GramBinary ListConst e1 e2) t       = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   env2 <- inferExpT env1 e2 (applySub (fst env1) (TList fresh1))
>   let sub = (fst env1) ++ (fst env2)
>   res <- unify (applySub sub t) (applySub sub (TList fresh1))
>   return (sub ++ res, snd env2)
> inferExpT env (GramUnary Minus e) t                = inferUnExprT env e t TInt
> inferExpT env (GramUnary LogicalNot e) t           = inferUnExprT env e t TBool
> inferExpT env (GramExpId (Var i _)) t              = Nothing
> inferExpT env (GramExpFunCall (GramFunCall i _)) t = Nothing
> inferExpT env (GramExpTuple e1 e2) t               = do
>   let fresh1 = fresh env
>   env1 <- inferExpT (inc env) e1 fresh1
>   let fresh2 = fresh env1
>   env2 <- inferExpT (inc env1) e2 fresh2
>   let sub = (fst env1) ++ (fst env2)
>   res  <- unify (applySub sub t) (applySub sub (TTuple fresh1 fresh2))
>   return (sub ++ res, snd env2)


> inferBinExprT :: Environment -> GramExp -> GramExp -> Type -> Type -> Type -> Maybe Environment
> inferBinExprT env e1 e2 texp telem tres = do
>   env1 <- inferExpT env e1 telem
>   env2 <- inferExpT env1 e2 (applySub (fst env1) telem)
>   let sub = (fst env1) ++ (fst env2)
>   res  <- unify (applySub sub texp) (applySub sub tres)
>   return (sub ++ res, snd env2)

> inferUnExprT :: Environment -> GramExp -> Type -> Type -> Maybe Environment
> inferUnExprT env e texp tcomp = do
>   env1 <- inferExpT env e tcomp
>   let sub = fst env1
>   res <- unify (applySub sub texp) (applySub sub tcomp)
>   return (sub ++ res, snd env1)



Occurs check

> occurs :: Int -> Type -> Bool
> occurs i (TTuple ta tb)          = (occurs i ta) || (occurs i tb)
> occurs i (TList t)               = occurs i t
> occurs i (TFunc [] tb)           = (occurs i tb)
> occurs i (TFunc (ta1:ta1s) tb)   = (occurs i ta1) || (occurs i (TFunc ta1s tb))
> occurs i (TVar j)                = i == j
> occurs _ _ = False


Substitution / environment manipulation

> fresh :: Environment -> Type
> fresh env                        = TVar (snd env)

> inc :: Environment -> Environment
> inc (sub, i)                     = (sub, i+1)

> liftMaybe :: (Maybe Substitutions, Int) -> Maybe Environment
> liftMaybe (Nothing, _)           = Nothing
> liftMaybe (Just sub, i)          = Just (sub, i)

> applySub :: Substitutions -> Type -> Type
> applySub [] t                    = t
> applySub sub (TTuple ta tb)      = TTuple (applySub sub ta) (applySub sub tb)
> applySub sub (TList t)           = TList (applySub sub t)
> applySub sub (TFunc tas tb)      = TFunc [applySub sub ta | ta <- tas] (applySub sub tb)
> applySub sub (TVar i)            = case lookup i sub of Just subbed -> subbed
>                                                         Nothing     -> (TVar i)
> applySub _ t                     = t