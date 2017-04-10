> import Grammar
> import Token 
> import Test

> data Type = BoolT | IntT | CharT | VoidT | TupleT Type Type
>   deriving (Show, Eq)

> type Environment = [(String, Type)]

> typeExp :: Environment -> GramExp -> Maybe Type
> typeExp env (GramBool _) = Just BoolT
> typeExp env (GramChar _) = Just CharT
> typeExp env (GramNum _)  = Just IntT
> typeExp env (GramBinary Minus e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary Plus e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary Times e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary Division e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary Mod e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary LessThan e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary LessOrEqual e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary GreaterThan e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary GreatherOrEqual e1 e2) = matchBinExprType e1 e2 IntT env
> typeExp env (GramBinary Equals e1 e2) = matchBinExpr e1 e2  env
> typeExp env (GramBinary Different e1 e2) = matchBinExpr e1 e2 env
> typeExp env (GramBinary LogicalOr e1 e2) = matchBinExprType e1 e2 BoolT env
> typeExp env (GramBinary LogicalAnd e1 e2) = matchBinExprType e1 e2 BoolT env
> typeExp env (GramUnary LogicalNot e) = matchExprType e BoolT env
> typeExp env (GramExpId (Var i _)) = lookup i env
> typeExp env (GramExpTuple e1 e2) = typeExp env e1 >>= (\x -> typeExp env e2 >>= (\y -> Just (TupleT x y)))

> matchExprType e t env = do
>   t1 <- typeExp env e
>   if t1 == t then (Just t1) else Nothing

> matchBinExpr e1 e2 env = do
>   t1 <- typeExp env e1
>   t2 <- typeExp env e2
>   if t1 == t2 then (Just t1) else Nothing

> matchBinExprType e1 e2 t env = do
>   t1 <- typeExp env e1
>   t2 <- typeExp env e2
>   if t1 == t && t2 == t then (Just t) else Nothing

