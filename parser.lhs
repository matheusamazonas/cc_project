> module Parser where

> import Text.ParserCombinators.Parsec.Prim hiding (satisfy)
> import Text.Parsec.Pos (SourcePos, newPos)
> import Text.Parsec.Combinator (optional, optionMaybe, many1)
> import Control.Monad (void)
> import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
> import Data.Maybe (maybeToList)
> import Lexer (lexer)
> import Grammar
> import Token

> type MyParser a = GenParser PosToken () a

Advance and satisfy were based on code from 
from http://osa1.net/posts/2012-08-30-separating-lexing-and-parsing-in-parsec.html

> advance :: SourcePos -> t -> [PosToken] -> SourcePos
> advance _ _ ((_, pos) : _) = pos
> advance pos _ [] = pos

> satisfy :: (PosToken -> Bool) -> MyParser Token
> satisfy f = tokenPrim show
>     advance
>     (\c -> if f c then Just (fst c) else Nothing)

Given a Token, parses any token of that kind. Uses the
function "matchToken" to compare them. Check below.

> isToken :: Token -> MyParser Token
> isToken t = (satisfy $ (matchToken t) . fst) <?> show t

Since  some tokens are parameterized, we can't jsut compare
them using == when trying to parse a token of a given type,
hence why we have this function. If we want to compare 
parameterized tokens, including their parameter, we can just
use == instead (or just pattern match, like in constField).

> matchToken :: Token -> Token -> Bool
> matchToken (TokenId _)   (TokenId _)   = True
> matchToken (TokenNum _)  (TokenNum _)  = True
> matchToken (TokenBool _) (TokenBool _) = True
> matchToken (TokenType _) (TokenType _) = True
> matchToken t1 t2 = t1 == t2

Parses a Bool. Pretty straightforward.

> pBool :: MyParser GramExp
> pBool = do
>   TokenBool b <- isToken (TokenBool True)
>   return (GramBool b)

Parses an Int. Keep in mind that the - is optional.

> pInt :: MyParser GramExp
> pInt = do
>   s <- optionMaybe $ isToken $ TokenOp Minus	
>   TokenNum b <- isToken (TokenNum 0)
>   case s of 
>     Nothing -> return (GramNum b)
>     Just _  -> return (GramNum (-b))

Parses a type. Self-explanatory 

> pType :: MyParser GramType
> pType = pBasicType <|> pTupleType <|> pListType <|> ((GramIdType) <$> pId)

Parses a basic type.
Grammar: BasicType = 'Int' | 'Bool' | 'Char'

> pBasicType :: MyParser GramType
> pBasicType = do
>   t <- pB BoolType <|> pB CharType <|> pB IntType
>   case t of 
>     TokenType x -> return (GramBasicType x)
>   where
>     pB b = isToken $ TokenType b

Parses a tuple type (not a tuple!). Examples:
     (a,b)           (Int, Bool)
Grammar: ’(’ Type ’,’ Type ’)’ 

> pTupleType :: MyParser GramType
> pTupleType = do
>   void $ isToken TokenOpenP
>   t1 <- pType
>   void $ isToken TokenComma
>   t2 <- pType
>   void $ isToken TokenCloseP
>   return (GramTupleType t1 t2)

Parses a list type (not a list!). Examples:
    [Int]         ;          [(Bool, Int)]  
Grammar: ’[’ Type ’]’

> pListType :: MyParser GramType
> pListType = do
>   void $ isToken TokenOpenSquareB
>   t <- pType
>   void $ isToken TokenCloseSquareB
>   return (GramListType t)

Parser for any TokenId. Returns the string inside the token
instead of the token itself because it's heavily used later.

> pId :: MyParser GramId
> pId = do
>   t <- isToken $ TokenId ""
>   case t of
>     TokenId s -> return (s)

Parser for Void return type. We need this parser just to
wrap the TokenVoid in the grammar's type for Void

> pVoidType :: MyParser GramRetType
> pVoidType = do 
>   t <- isToken TokenVoidType
>   return (GramVoidType)

Parser for function return types.
Grammar: Type | 'Void'

> pRetType :: MyParser GramRetType
> pRetType = (GramRetType) <$> pType <|> pVoidType

Parser for function type full signature. Functional 
Programming-style functions (->). Example: 
        Int Int Char Bool -> Int
Grammar: FunType = [ FTypes ] '->' RetType

> pFunType :: MyParser GramFunType
> pFunType = do
>   opt <- optionMaybe pFTypes
>   void $ isToken TokenFuncType 
>   r <- pRetType
>   let fType = maybeToList opt
>   return (GramFunType fType r)

Parser for function type list.
Example: Int Int Char Bool
Grammar: FTypes = Type [ FTypes ]

> pFTypes :: MyParser GramFTypes
> pFTypes = do
>   p <- pType 
>   opt <- optionMaybe pFTypes
>   let fTypes = maybeToList opt
>   return (GramFTypes p fTypes)

Parser for function arguments
Grammar: FArgs = id [ ',' FArgs ]

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

Parser for arguments list. Used on function calls (Expr8)
Grammar: ArgList = '(' [ ActArgs ] ')'

> pArgList = do
>   void $ isToken TokenOpenP
>   opt <- optionMaybe pActArgs
>   void $ isToken TokenCloseP
>   let args = maybeToList opt
>   return (GramArgList args)

Root parser for Expressions. 
Parser for binary operator || (or)
Grammar: Exp = Exp1 [ '||' Exp ]

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

Parser for binary operator && (and)
Grammar: Epx1 = Exp2 [ '&&' Exp1 ]

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

Parser for relational operators (<. >, <=, >=, ==, !=)
Grammar: Exp2 = Exp3 [ ('==' | '!=' | '<' | '<=' | '>' | '>=') Exp2 ]

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

Parser for lists
Grammar: Exp3 = Exp4 [  ':' Exp3 ]

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
Parser for binary operators + and -
Grammar: Exp4 = Exp5 [ (('+' | '-') Exp5)* ]

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
Parser for binary operators *, / and %
Grammar: Exp5 = Exp6 [ (('*' | '/' | '%') Exp6)* ]

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

Parser for unary operators !(boolean not) and -(int negation)
Grammar: Exp6 = [ ('!' | '-') ] Exp7

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

Grammar: Exp7 = int | char | 'False' | 'True' | '[]' | Exp8 | Exp9

> pExpr7 :: MyParser GramExp
> pExpr7 = pInt <|> pBool <|> pExpr8 <|> pExpr9 <|> pEmptyList

This parser was created to apply left refactoring on Expr7, since
2 rules contained "id" is a common prefix: (id [Field]) and FunCall.
Grammar: Exp8 = id [Field | ArgList]

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

Used to assign high precedence for expressions inside parenthesis
Grammar: Exp9 = '(' Exp [ ',' Exp ] ')'

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

> pEmptyList :: MyParser GramExp
> pEmptyList = do
>   void $ isToken TokenOpenSquareB
>   void $ isToken TokenCloseSquareB
>   return (GramEmptyList)

Grammar: ActArgs = Exp [ ',' ActArgs ]

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

Since the grammar-defined fields are just TokenIds in the
lexer, we need to match them manually in the parser. This
method returns a Maybe constructor for TokenIds. If the 
id is one of the reserved ones, it returns a constructor 
and if not, returns Nothing (which should never occur in
a well formed program).

> constField :: Token -> Maybe ([GramField] -> GramField)
> constField (TokenId "hd")  = Just Head 
> constField (TokenId "tl")  = Just Tail 
> constField (TokenId "fst") = Just First 
> constField (TokenId "snd") = Just Second
> constField _               = Nothing 

The SPL has a limited, well-defined list of fields available:
hd (head), tl (tail), fst (first) and snd (second). Every 
field starts with a TokenPeriod and is followed by one of 
the defined fields. We make use of the constField function
above to easily construct GramField based on TokenIds. If the
field isn't one of the reserved ones, it throwns an error. 

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
>       Nothing -> error "Invalid field"

This parser is self-explanatory.

> pStmt :: MyParser GramStmt
> pStmt = pIf <|> pWhile <|> pStmt1 <|> pReturn

Here a common prefix problem arises because both attribution and
function call start with "id". So, we left refactor this rule by
creating a new rule Stmt1 that consumes an id and checks what comes
after it. It can either be a list of arguments (pArgList) or an 
attribution body (pAttribBody)

> pStmt1 :: MyParser GramStmt
> pStmt1 = do
>   i <- pId
>   opt <- (Left) <$> pAttribBody <|> (Right) <$> pArgList
>   void $ isToken TokenEOL
>   case opt of
>     Left (field, expr) -> return (GramAttr (Var i field) expr)
>     Right args -> return (GramStmtFunCall (GramFunCall i args))

An attribution body is anything that comes after the variable name:
         " [Field] '='' Expr ';'' "

> pAttribBody :: MyParser ([GramField], GramExp)
> pAttribBody = do
>   optField <- optionMaybe pField
>   void $ isToken TokenAttribution
>   expr <- pExpr
>   let field = maybeToList optField
>   return (field, expr)

Grammar: 'if' '(' Exp ')' '{' Stmt* '}' [ 'else' '{' Stmt* '}' ]

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

pElse is used just internally in pIf because the ELSE part is 
optional.

> pElse :: MyParser [GramStmt]
> pElse = do
>   void $ isToken TokenElse
>   void $ isToken TokenOpenCurlyB
>   stmt <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   return (stmt)

Grammar: 'while' '(' Exp ')' '{' Stmt* '}'

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

Grammar: 'return' [ Exp ] ';'

> pReturn :: MyParser GramStmt
> pReturn = do
>   void $ isToken TokenReturn
>   opt <- optionMaybe pExpr
>   void $ isToken TokenEOL
>   let expr = maybeToList opt
>   return (GramReturn expr)

The parser pVarDecl is not used in Decl. It's used inside FuncDeclTail 
in the grammar rule that says that a function may have many variables
declared inside it.

> pVarDecl :: MyParser GramVarDecl
> pVarDecl = pVarDeclVar <|> pVarDeclCustom

The parser pVarDeclCustom is only used by pVarDecl, it's not used in 
pDecl. Check comments above.

> pVarDeclCustom :: MyParser GramVarDecl
> pVarDeclCustom = do
>   i <- pType
>   t <- pVarDeclTail
>   return (GramVarDeclType i t)

The parser pVarDeclVar is only used by pVarDecl, it's not used in pDecl. 
Chech comments above.

> pVarDeclVar :: MyParser GramVarDecl
> pVarDeclVar = do
>   op <- isToken TokenVar
>   t <- pVarDeclTail
>   return (GramVarDeclVar t)

Here starts the parsers involved with Decl. Decl can be either a VarDecl
(variable declaration) or a FunDecl (function declaration). A hidden 
problem arises here. FunDecl and VarDecl may share a common prefix: id. 
So, in order to left refactor this rule, we need to take "id" as a common
prefix of those rules. To do that, we need to break FunDecl and VarDecl 
into head (id) and tail, which are the parsers you see below.

We need a function to take a GramID (String) out of a getTypeId because
"id" is also a "Type", and when we try to parse a Type, ids also match.

> getTypeId :: GramType -> GramId
> getTypeId (GramIdType i) = i
> getTypeId _ = error "getTypeId is supposed to work ony with GramTypeId"

Keep in mind that a VarDecl can be two things: either 
        "var x = 1;" or
        "Type x = 1;"
So we need to check for both. We do it in pDecl.
If the first Token is a TokenVar, then definitely it's a VarDecl. But if
it's an TokenId, it can either be a VarDecl of a custom type or a FunDecl.
We can only know which one it is by parsing the remainder (tail). Once we 
do, we can return the correct result.
Grammar: Decl = (Type Decl1) | ('var' VarDeclTail)
         Decl1 = FunDeclTail | (id VarDeclTail)

> pDecl :: MyParser GramDecl
> pDecl = do
>     declHead <- (Left) <$> pType <|> (Right) <$> isToken TokenVar 
>     case declHead of
>       Left declType -> pDecl1 declType
>       Right _ -> pDecl2

pDecl parses any part after a TokenType. It can either be a function call
or a definition of a variable with a given type (without using "var").

> pDecl1 :: GramType -> MyParser GramDecl
> pDecl1 declType = do
>   tail2 <- (Left) <$> pVarDeclTail <|> (Right) <$> pFuncDeclTail	
>   case tail2 of
>     Left v -> return (GramDeclVar (GramVarDeclType declType v))	
>     Right f -> return (GramDeclFun (GramFuncDecl (getTypeId declType) f))	

pDecl2 parses VarDecl. Since the TokenVar should've been parsed before
this is called and that it's useless for the Syntax Tree, we don't need it.

> pDecl2 :: MyParser GramDecl
> pDecl2 = do
>   tail1 <- pVarDeclTail
>   return (GramDeclVar (GramVarDeclVar tail1)) 

> pVarDeclTail :: MyParser GramVarDeclTail
> pVarDeclTail = do
>   i <- pId
>   void $ isToken TokenAttribution
>   expr <- pExpr
>   return (GramVarDeclTail i expr)

> pFuncDeclTail :: MyParser GramFuncDeclTail
> pFuncDeclTail = do
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
>       in return (GramFuncDeclTail args fType vars stmts)
>   where
>     pOptType = do
>       void $ isToken TokenFuncDecl
>       t <- pFunType
>       return (t)	

This is the root parser and is the one that should be used to parse a 
full program.
Grammar: SPL = Decl+

> pSPL :: MyParser Gram
> pSPL = many1 pDecl






















