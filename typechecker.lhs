> import Grammar
> import Token 
> import Test

> data Type = BoolT | IntT | CharT | VoidT 
>   deriving (Show, Eq)

> typeExp :: GramExp -> Maybe Type
> typeExp (GramBool _) = Just BoolT
> typeExp (GramChar _) = Just CharT
> typeExp (GramNum _)  = Just IntT
> typeExp (GramBinary Minus e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary Plus e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary Times e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary Division e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary Mod e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary LessThan e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary LessOrEqual e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary GreaterThan e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary GreatherOrEqual e1 e2) = matchBinExprType e1 e2 IntT
> typeExp (GramBinary Equals e1 e2) = matchBinExpr e1 e2
> typeExp (GramBinary Different e1 e2) = matchBinExpr e1 e2
> typeExp (GramBinary LogicalOr e1 e2) = matchBinExprType e1 e2 BoolT
> typeExp (GramBinary LogicalAnd e1 e2) = matchBinExprType e1 e2 BoolT
> typeExp (GramUnary LogicalNot e) = matchExprType e BoolT


> matchExprType e t = do
>   t1 <- typeExp e
>   if t1 == t then (Just t1) else Nothing

> matchBinExpr e1 e2 = do
>   t1 <- typeExp e1
>   t2 <- typeExp e2
>   if t1 == t2 then (Just t1) else Nothing

> matchBinExprType e1 e2 t = do
>   t1 <- typeExp e1
>   t2 <- typeExp e2
>   if t1 == t && t2 == t then (Just t) else Nothing

