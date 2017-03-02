> module Parser where

> import Text.ParserCombinators.Parsec.Prim hiding (satisfy)
> import Text.Parsec.Pos (SourcePos, newPos)
> import Text.Parsec.Combinator (optional, optionMaybe, many1)
> import Control.Monad (void)
> import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
> import Data.Maybe
> import Lexer

> type MyParser a = GenParser PosToken () a

> type GramId = String

> type Gram = [GramDecl]

> data GramVar = Var GramId [GramField]
>   deriving (Show, Eq)

> data GramFunType = 
>      GramFunType [GramFTypes] GramRetType
>   deriving (Show, Eq)

> data GramFTypes =
>      GramFTypes GramType [GramFTypes]
>   deriving (Show, Eq)

> data GramRetType =
>      GramRetType GramType
>    | GramVoidType
>   deriving (Show, Eq)

> data GramType =
>      GramBasicType BasicType
>    | GramTupleType GramType GramType
>    | GramListType GramType
>    | GramIdType GramId
>   deriving (Show, Eq)

> data GramFArgs = 
>      GramFArgsId GramId [GramFArgs]
>   deriving (Show, Eq)

> data GramExp = 
>      GramBinary Operation GramExp GramExp
>    | GramUnary Operation GramExp 
>    | GramBool Bool
>    | GramNum Int
>    | GramExpFunCall GramFunCall
>    | GramExpId GramVar
>    | GramExpTuple GramExp GramExp
>   deriving (Show, Eq)

> data GramArgList = GramArgList [GramActArgs]
>   deriving (Show, Eq)

> data GramFunCall = 
>     GramFunCall GramId GramArgList
>   deriving (Show, Eq)

> data GramActArgs = GramActExpr GramExp [GramActArgs]
>   deriving (Show, Eq)

> data GramField = 
>      First [GramField] 
>    | Second [GramField] 
>    | Head [GramField]
>    | Tail [GramField]
>   deriving (Show, Eq)

> data GramStmt = 
>      GramIf GramExp [GramStmt] [GramStmt]
>    | GramWhile GramExp [GramStmt]
>    | GramAttr GramVar GramExp
>    | GramStmtFunCall GramFunCall
>    | GramReturn [GramExp] 
>   deriving (Show, Eq)   

> data GramVarDecl = 
>      GramVarDeclVar GramId GramExp
>    | GramVarDeclType GramType GramId GramExp
>   deriving (Show, Eq)

> data GramFuncDecl = 
>      GramFuncDecl GramId [GramFArgs] [GramFunType] [GramVarDecl] [GramStmt]
>   deriving (Show, Eq)

> data GramDecl = 
>      GramDeclFun GramFuncDecl
>    | GramDeclVar GramVarDecl
>   deriving (Show, Eq)

Advance and satisfy were based on code from 
from http://osa1.net/posts/2012-08-30-separating-lexing-and-parsing-in-parsec.html

> advance :: SourcePos -> t -> [PosToken] -> SourcePos
> advance _ _ ((_, pos) : _) = pos
> advance pos _ [] = pos

> satisfy :: (PosToken -> Bool) -> MyParser Token
> satisfy f = tokenPrim show
>     advance
>     (\c -> if f c then Just (fst c) else Nothing)

> isToken :: Token -> MyParser Token
> isToken t = (satisfy $ (matchToken t) . fst) <?> show t

> matchToken :: Token -> Token -> Bool
> matchToken (TokenId _)   (TokenId _)   = True
> matchToken (TokenNum _)  (TokenNum _)  = True
> matchToken (TokenBool _) (TokenBool _) = True
> matchToken (TokenType _) (TokenType _) = True
> matchToken t1 t2 = t1 == t2

> pBool :: MyParser GramExp
> pBool = do
>   TokenBool b <- isToken (TokenBool True)
>   return (GramBool b)

> pInt :: MyParser GramExp
> pInt = do
>   s <- optionMaybe $ isToken $ TokenOp Minus	
>   TokenNum b <- isToken (TokenNum 0)
>   case s of 
>     Nothing -> return (GramNum b)
>     Just _  -> return (GramNum (-b))

> pType :: MyParser GramType
> pType = pBasicType <|> pTupleType <|> pListType <|> ((GramIdType) <$> pId)

> pBasicType :: MyParser GramType
> pBasicType = do
>   t <- pB BoolType <|> pB CharType <|> pB IntType
>   case t of 
>     TokenType x -> return (GramBasicType x)
>   where
>     pB b = isToken $ TokenType b

> pTupleType :: MyParser GramType
> pTupleType = do
>   void $ isToken TokenOpenP
>   t1 <- pType
>   void $ isToken TokenComma
>   t2 <- pType
>   void $ isToken TokenCloseP
>   return (GramTupleType t1 t2)

> pListType :: MyParser GramType
> pListType = do
>   void $ isToken TokenOpenSquareB
>   t <- pType
>   void $ isToken TokenCloseSquareB
>   return (GramListType t)

> pId :: MyParser GramId
> pId = do
>   t <- isToken $ TokenId ""
>   case t of
>     TokenId s -> return (s)

> pVoidType :: MyParser GramRetType
> pVoidType = do 
>   t <- isToken TokenVoidType
>   return (GramVoidType)

> pRetType :: MyParser GramRetType
> pRetType = ((GramRetType) <$> pType) <|>  pVoidType

> pFTypes :: MyParser GramFTypes
> pFTypes = do
>   p <- pType 
>   opt <- optionMaybe pFTypes
>   let fTypes = maybeToList opt
>   return (GramFTypes p fTypes)

> pFunType :: MyParser GramFunType
> pFunType = do
>   opt <- optionMaybe pFTypes
>   void $ isToken TokenFuncType 
>   r <- pRetType
>   let fType = maybeToList opt
>   return (GramFunType fType r)

> pFArgs :: MyParser GramFArgs
> pFArgs = do
>   i <- pId
>   opt <- optionMaybe $ pOp
>   let args = maybeToList opt
>   return (GramFArgsId i args)
>   where 
>     pOp = do
>       void $ isToken TokenComma
>       f <- pFArgs
>       return (f)

TODO: Add ActArgs
TODO: Add ActArgs to pFunCall.

> pArgList = do
>   void $ isToken TokenOpenP
>   opt <- optionMaybe pActArgs
>   void $ isToken TokenCloseP
>   let args = maybeToList opt
>   return (GramArgList args)

> pFunCall :: MyParser GramFunCall
> pFunCall = do
>   i <- pId
>   args <- pArgList
>   return (GramFunCall i args)

> pExpr :: MyParser GramExp
> pExpr = do
>   e1 <- pExpr1
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pEquals = isToken $ TokenOp LogicalOr
>     pOpt = do
>       op <- pEquals
>       e2 <- pExpr
>       return(op, e2)

> pExpr1 :: MyParser GramExp
> pExpr1 = do
>   e1 <- pExpr2
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pEquals = isToken $ TokenOp LogicalAnd
>     pOpt = do
>       op <- pEquals
>       e2 <- pExpr1
>       return(op, e2)

> pExpr2 :: MyParser GramExp
> pExpr2 = do
>   e1 <- pExpr3
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pEquals = isToken $ TokenOp Equals
>     pDifferent = isToken $ TokenOp Different
>     pLess = isToken $ TokenOp LessThan
>     pLessEq = isToken $ TokenOp LessOrEqual
>     pGreater = isToken $ TokenOp GreaterThan
>     pGreaterEq = isToken $ TokenOp GreatherOrEqual
>     pOpt = do
>       op <- pEquals <|> pDifferent <|> pLess <|> pLessEq <|> pGreater <|> pGreaterEq
>       e2 <- pExpr2 
>       return(op, e2)

> pExpr3 :: MyParser GramExp
> pExpr3 = do
>   e1 <- pExpr4
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pList = isToken $ TokenOp ListConst
>     pOpt = do
>       op <- pList
>       e2 <- pExpr3 
>       return(op, e2)

TODO: Change optional to many. Maybe using fmap (<$>)?

> pExpr4 :: MyParser GramExp
> pExpr4 = do
>   e1 <- pExpr5
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pPlus = isToken $ TokenOp Plus
>     pMinus = isToken $ TokenOp Minus
>     pOpt = do
>       op <- pPlus <|> pMinus
>       e2 <- pExpr5 
>       return(op, e2)

TODO: Change optional to many. Maybe using fmap (<$>)?

> pExpr5 :: MyParser GramExp
> pExpr5 = do
>   e1 <- pExpr6
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (e1)
>     Just (TokenOp op, e2) -> return (GramBinary op e1 e2)
>   where
>     pTimes = isToken $ TokenOp Times
>     pDivision = isToken $ TokenOp Division
>     pMod = isToken $ TokenOp Mod
>     pOpt = do
>       op <- pTimes <|> pDivision <|> pMod
>       e2 <- pExpr6 
>       return(op, e2)

> pExpr6 :: MyParser GramExp
> pExpr6 = do
>   opt <- optionMaybe (oNot <|> oMinus)
>   e <- pExpr7
>   case opt of
>     Nothing -> return (e)
>     Just (TokenOp o) -> return (GramUnary o e)
>   where
>     oNot = isToken $ TokenOp LogicalNot
>     oMinus = isToken $ TokenOp Minus

> pExpr7 :: MyParser GramExp
> pExpr7 = pInt <|> pBool <|> pExpr8 <|> pExpr9

> pExpr8 :: MyParser GramExp
> pExpr8 = do
>   i <- pId
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return (GramExpId (Var i []))
>     Just o -> do
>     case o of	
>       Left args -> return (GramExpFunCall (GramFunCall i args))
>       Right field -> return (GramExpId (Var i [field]))
>   where 
>     pOpt = (Left) <$> pArgList <|> (Right) <$> pField

> pExpr9 :: MyParser GramExp
> pExpr9 = do
>   void $ isToken TokenOpenP
>   e1 <- pExpr
>   opt <- optionMaybe pOpt
>   void $ isToken TokenCloseP
>   case opt of
>     Nothing -> return e1
>     Just e2 -> return (GramExpTuple e1 e2)	
>   where
>     pOpt = do
>       void $ isToken TokenComma
>       e2 <- pExpr
>       return (e2)	

> pActArgs :: MyParser GramActArgs
> pActArgs = do
>   e <- pExpr
>   opt <- optionMaybe pOpt
>   let args = maybeToList opt
>   return (GramActExpr e args)
>   where
>     pOpt = do
>       void $ isToken TokenComma
>       args <- pActArgs
>       return args

> constField :: Token -> Maybe ([GramField] -> GramField)
> constField (TokenId "hd")  = Just Head 
> constField (TokenId "tl")  = Just Tail 
> constField (TokenId "fst") = Just First 
> constField (TokenId "snd") = Just Second
> constField _               = Nothing 

> pField :: MyParser GramField
> pField = do
>   void $ isToken TokenPeriod
>   pId <- isToken $ TokenId ""
>   let maybeConst = constField pId in
>     case maybeConst of 
>       Just constructor -> do
>         opt <- optionMaybe pField
>         let field = maybeToList opt
>         return (constructor field)

> pStmt :: MyParser GramStmt
> pStmt = pIf <|> pWhile <|> pStmt1 <|> pReturn

> pStmt1 :: MyParser GramStmt
> pStmt1 = do
>   i <- pId
>   opt <- (Left) <$> pAttribBody <|> (Right) <$> pArgList
>   void $ isToken TokenEOL
>   case opt of
>     Left (field, expr) -> return (GramAttr (Var i field) expr)
>     Right args -> return (GramStmtFunCall (GramFunCall i args))

> pAttribBody :: MyParser ([GramField], GramExp)
> pAttribBody = do
>   optField <- optionMaybe pField
>   void $ isToken TokenAttribution
>   expr <- pExpr
>   let field = maybeToList optField
>   return (field, expr)

> pIf :: MyParser GramStmt
> pIf = do
>   void $ isToken TokenIf
>   void $ isToken TokenOpenP
>   expr <- pExpr
>   void $ isToken TokenCloseP
>   void $ isToken TokenOpenCurlyB
>   stmt <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   optElse <- optionMaybe pElse
>   case optElse of
>     Nothing -> return (GramIf expr stmt [])
>     Just e  -> return (GramIf expr stmt e)

> pElse :: MyParser [GramStmt]
> pElse = do
>   void $ isToken TokenElse
>   void $ isToken TokenOpenCurlyB
>   stmt <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   return (stmt)

> pWhile :: MyParser GramStmt
> pWhile = do
>   void $ isToken TokenWhile
>   void $ isToken TokenOpenP
>   expr <- pExpr
>   void $ isToken TokenCloseP 	
>   void $ isToken TokenOpenCurlyB
>   stmt <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   return (GramWhile expr stmt)

> pReturn :: MyParser GramStmt
> pReturn = do
>   void $ isToken TokenReturn
>   opt <- optionMaybe pExpr
>   void $ isToken TokenEOL
>   let expr = maybeToList opt
>   return (GramReturn expr)

> pVarDecl :: MyParser GramVarDecl
> pVarDecl = do
>   op <- (Left) <$> isToken TokenVar <|> (Right) <$> pType
>   i <- pId
>   expr <- pExpr
>   case op of 
>     Left _ -> return (GramVarDeclVar i expr)
>     Right t -> return (GramVarDeclType t i expr)

> pFuncDecl :: MyParser GramFuncDecl
> pFuncDecl = do
>   i <- pId
>   void $ isToken TokenOpenP
>   optFArgs <- optionMaybe pFArgs
>   void $ isToken TokenCloseP
>   optFunType <- optionMaybe pOptType
>   void $ isToken TokenOpenCurlyB
>   vars <- many pVarDecl
>   stmts <- many1 pStmt
>   void $ isToken TokenCloseCurlyB
>   let args = maybeToList optFArgs
>       fType = maybeToList optFunType 
>       in return (GramFuncDecl i args fType vars stmts)
>   where
>     pOptType = do
>       void $ isToken TokenFuncDecl
>       t <- pFunType
>       return (t)	

> pDecl :: MyParser GramDecl
> pDecl = (GramDeclVar) <$> pVarDecl <|> (GramDeclFun) <$> pFuncDecl

> pSPL :: MyParser Gram
> pSPL = many1 pDecl























