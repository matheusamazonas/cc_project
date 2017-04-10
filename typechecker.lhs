> import Grammar
> import Token 

> data Type = BoolT | IntT | CharT | VoidT | TupleT Type Type | ListT Type
>   deriving (Show, Eq)

> type Environment = [(String, Type)]

> inferExpT :: Environment -> GramExp -> Maybe Type
> inferExpT env (GramBool _) = Just BoolT
> inferExpT env (GramChar _) = Just CharT
> inferExpT env (GramNum _)  = Just IntT
> inferExpT env (GramBinary Minus e1 e2)           = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary Plus e1 e2)            = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary Times e1 e2)           = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary Division e1 e2)        = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary Mod e1 e2)             = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary LessThan e1 e2)        = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary LessOrEqual e1 e2)     = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary GreaterThan e1 e2)     = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary GreatherOrEqual e1 e2) = matchBinExprType env e1 e2 IntT 
> inferExpT env (GramBinary Equals e1 e2)          = matchBinExpr env e1 e2  
> inferExpT env (GramBinary Different e1 e2)       = matchBinExpr env e1 e2 
> inferExpT env (GramBinary LogicalOr e1 e2)       = matchBinExprType env e1 e2 BoolT 
> inferExpT env (GramBinary LogicalAnd e1 e2)      = matchBinExprType env e1 e2 BoolT
> inferExpT env (GramUnary LogicalNot e)           = matchExprType env e BoolT
> inferExpT env (GramExpId (Var i _))              = lookup i env
> inferExpT env (GramExpFunCall (GramFunCall i _)) = lookup i env
> inferExpT env (GramExpTuple e1 e2) = inferExpT env e1 >>= (\x -> inferExpT env e2 >>= (\y -> Just (TupleT x y)))


> checkExpT :: Environment -> GramExp -> Type -> Bool
> checkExpT env e t = inferExpT env e == Just t

> matchExprType :: Environment -> GramExp -> Type -> Maybe Type
> matchExprType env e t = do
>   t1 <- inferExpT env e
>   if t1 == t then (Just t1) else Nothing

> matchBinExpr :: Environment -> GramExp -> GramExp -> Maybe Type
> matchBinExpr env e1 e2 = do
>   t1 <- inferExpT env e1
>   t2 <- inferExpT env e2
>   if t1 == t2 then (Just t1) else Nothing

> matchBinExprType :: Environment -> GramExp -> GramExp -> Type -> Maybe Type
> matchBinExprType env e1 e2 t = do
>   t1 <- inferExpT env e1
>   t2 <- inferExpT env e2
>   if t1 == t && t2 == t then (Just t) else Nothing

