> module Parser where

> import Text.ParserCombinators.Parsec.Prim hiding (satisfy)
> import Text.Parsec.Pos (SourcePos, newPos)
> import Text.Parsec.Combinator (optional, optionMaybe, many1, chainl1, chainl, sepEndBy)
> import Control.Monad (void)
> import Control.Applicative ((<$>))
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

> satisfy :: (PosToken -> Bool) -> MyParser PosToken
> satisfy f = tokenPrim show
>     advance
>     (\c -> if f c then Just c else Nothing)

Given a Token, parses any token of that kind. Uses the
function "matchToken" to compare them. Check below.

> isToken :: Token -> MyParser PosToken
> isToken t = (satisfy $ (matchToken t) . fst) <?> show t

Since  some tokens are parameterized, we can't jsut compare
them using == when trying to parse a token of a given type,
hence why we have this function. If we want to compare 
parameterized tokens, including their parameter, we can just
use == instead (or just pattern match, like in constField).

> matchToken :: Token -> Token -> Bool
> matchToken (TokenId   _) (TokenId _)   = True
> matchToken (TokenNum  _) (TokenNum _)  = True
> matchToken (TokenBool _) (TokenBool _) = True
> matchToken (TokenChar _) (TokenChar _) = True
> matchToken (TokenType _) (TokenType _) = True
> matchToken t1 t2 = t1 == t2

Parses a Bool. Remember that the isToken function uses the 
matchToken and  that the value we send to it is a dummy value. 
So here, True doesn't mean anything. It's used just to confirm 
that the token is a TokenBool

> pBool :: MyParser GramExp
> pBool = do
>   (TokenBool b, p) <- isToken (TokenBool True)
>   return $ GramBool p b

Parses a Char. Remember that the isToken function uses the 
matchToken and  that the value we send to it is a dummy value. 
So here, ' ' doesn't mean anything. It's used just to confirm 
that the token is a TokenChar

> pChar :: MyParser GramExp
> pChar = do
>   (TokenChar c, p) <- isToken $ TokenChar ' '
>   return $ GramChar p c

Parses an Int. Keep in mind that the - is optional. Also, 
remember that the isToken function uses the matchToken and 
that the value we send to it is a dummy value. So here, 0 
doesn't mean anything. It's used just to confirm that the 
token is a TokenNum

> pInt :: MyParser GramExp
> pInt = do
>   optMinus <- optionMaybe $ isToken $ TokenOp Minus	
>   (TokenNum num, p) <- isToken (TokenNum 0)
>   case optMinus of 
>     Nothing -> return $ GramNum p num
>     Just _  -> return $ GramNum p (-num)

Parses a type. Since pId returns a GramId (String), we 
need to cosntruct a GramIdType by mapping the constructor
on the GramId

> pType :: MyParser GramType
> pType = pBasicType <|> pTupleType <|> pListType <|> ((GramIdType) <$> pId)

Parses a basic type. Remember that the isToken function
uses the matchToken and that the value we send to it 
is a dummy value. So here, BoolType doesn't mean anything.
It's used just to confirm that the token is a TokenType
Grammar: BasicType = 'Int' | 'Bool' | 'Char'

> pBasicType :: MyParser GramType
> pBasicType = do
>   (TokenType t, p) <- isToken $ TokenType BoolType 
>   return $ GramBasicType p t

Parses a tuple type (not a tuple!). Examples:
     (a,b)           (Int, Bool)
Grammar: ’(’ Type ’,’ Type ’)’ 

> pTupleType :: MyParser GramType
> pTupleType = do
>   (_, p) <- isToken TokenOpenP
>   t1 <- pType
>   void $ isToken TokenComma
>   t2 <- pType
>   void $ isToken TokenCloseP
>   return $ GramTupleType p t1 t2

Parses a list type (not a list!). Examples:
    [Int]         ;          [(Bool, Int)]  
Grammar: ’[’ Type ’]’

> pListType :: MyParser GramType
> pListType = do
>   (_, p) <- isToken TokenOpenSquareB
>   t <- pType
>   void $ isToken TokenCloseSquareB
>   return $ GramListType p t

Parser for any TokenId. Returns the string inside the token
instead of the token itself because it's heavily used later.

> pId :: MyParser GramId
> pId = do
>   (TokenId i, p) <- isToken $ TokenId ""
>   return $ Id p i

Parser for Void return type. We need this parser just to
wrap the TokenVoid in the grammar's type for Void

> pVoidType :: MyParser GramRetType
> pVoidType = do 
>   (_, p) <- isToken TokenVoidType
>   return $ GramVoidType p

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
>   retType <- pRetType
>   let fType = maybeToList opt in
>     return $ GramFunType fType retType

Parser for function type list.
Example: Int Int Char Bool
Grammar: FTypes = Type [ FTypes ]

> pFTypes :: MyParser GramFTypes
> pFTypes = do
>   t <- pType 
>   opt <- optionMaybe pFTypes
>   let fTypes = maybeToList opt in
>     return $ GramFTypes t fTypes

Parser for function arguments
Grammar: FArgs = id [ ',' FArgs ]

> pFArgs :: MyParser GramFArgs
> pFArgs = do
>   (Id i p) <- pId
>   opt <- optionMaybe $ pOpt
>   let args = maybeToList opt in
>     return $ GramFArgsId (Id i p) args 
>   where 
>     pOpt = do
>       void $ isToken TokenComma
>       fArgs <- pFArgs
>       return fArgs

Parser for arguments list. Used on function calls (Expr8)
Grammar: ArgList = '(' [ ActArgs ] ')'

> pArgList :: MyParser [GramExp]
> pArgList = do
>   void $ isToken TokenOpenP
>   opt <- sepEndBy pExpr (isToken TokenComma) 
>   void $ isToken TokenCloseP
>   return  opt

Root parser for Expressions. 
Parser for binary operator || (or)
Grammar: Exp = Exp1 [ '||' Exp ]

> pExpr :: MyParser GramExp
> pExpr = do
>   e1 <- pExpr1
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return e1
>     Just (TokenOp op, e2, p) -> return $ GramBinary p op e1 e2
>   where
>     pEquals = isToken $ TokenOp LogicalOr
>     pOpt = do
>       (op, p) <- pEquals
>       e2 <- pExpr
>       return (op, e2, p)

Parser for binary operator && (and)
Grammar: Epx1 = Exp2 [ '&&' Exp1 ]

> pExpr1 :: MyParser GramExp
> pExpr1 = do
>   e1 <- pExpr2
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return e1
>     Just (TokenOp op, e2, p) -> return $ GramBinary p op e1 e2 
>   where
>     pEquals = isToken $ TokenOp LogicalAnd
>     pOpt = do
>       (op, p) <- pEquals
>       e2 <- pExpr1
>       return (op, e2, p)

Parser for relational operators (<. >, <=, >=, ==, !=)
Grammar: Exp2 = Exp3 [ ('==' | '!=' | '<' | '<=' | '>' | '>=') Exp2 ]

> pExpr2 :: MyParser GramExp
> pExpr2 = do
>   e1 <- pExpr3
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return e1
>     Just (TokenOp op, e2, p) -> return $ GramBinary p op e1 e2
>   where
>     pEquals = isToken $ TokenOp Equals
>     pDifferent = isToken $ TokenOp Different
>     pLess = isToken $ TokenOp LessThan
>     pLessEq = isToken $ TokenOp LessOrEqual
>     pGreater = isToken $ TokenOp GreaterThan
>     pGreaterEq = isToken $ TokenOp GreatherOrEqual
>     pOpt = do
>       (op, p) <- pEquals <|> pDifferent <|> pLess <|> pLessEq <|> pGreater <|> pGreaterEq
>       e2 <- pExpr2 
>       return(op, e2, p)

Parser for lists
Grammar: Exp3 = Exp4 [  ':' Exp3 ]

> pExpr3 :: MyParser GramExp
> pExpr3 = do
>   e1 <- pExpr4
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return e1
>     Just (TokenOp op, e2, p) -> return $ GramBinary p op e1 e2 
>   where
>     pList = isToken $ TokenOp ListConst
>     pOpt = do
>       (op, p) <- pList
>       e2 <- pExpr3 
>       return(op, e2, p)

Parser for binary operators + and -
Grammar: Exp4 = Exp5 [ (('+' | '-') Exp5)* ]

> pExpr4 :: MyParser GramExp
> pExpr4 = chainl1 pExpr5 pOp
>   where
>     pPlus = isToken $ TokenOp Plus
>     pMinus = isToken $ TokenOp Minus
>     pOp = do
>       (TokenOp o, p) <- pPlus <|> pMinus
>       return $ GramBinary p o

Parser for binary operators *, / and %
Grammar: Exp5 = Exp6 [ (('*' | '/' | '%') Exp6)* ]

> pExpr5 :: MyParser GramExp
> pExpr5 = chainl1 pExpr6 pOp
>   where
>     pTimes = isToken $ TokenOp Times
>     pDivision = isToken $ TokenOp Division
>     pMod = isToken $ TokenOp Mod
>     pOp = do
>       (TokenOp o, p) <- pTimes <|> pDivision <|> pMod
>       return $ GramBinary p o

Parser for unary operators !(boolean not) and -(int negation)
Grammar: Exp6 = [ ('!' | '-') ] Exp7

> pExpr6 :: MyParser GramExp
> pExpr6 = do
>   opt <- optionMaybe (oNot <|> oMinus)
>   expr <- pExpr7
>   case opt of
>     Nothing -> return (expr)
>     Just (TokenOp op, p) -> return $ GramUnary p op expr
>   where
>     oNot = isToken $ TokenOp LogicalNot
>     oMinus = isToken $ TokenOp Minus

Grammar: Exp7 = int | char | 'False' | 'True' | '[]' | Exp8 | Exp9

> pExpr7 :: MyParser GramExp
> pExpr7 = pInt <|> pBool <|> pExpr8 <|> pExpr9 <|> pEmptyList <|> pChar

This parser was created to left refactor Expr7, since 2 rules 
contained "id" is a common prefix: (id [Field]) and FunCall.
Grammar: Exp8 = id [Field | ArgList]

> pExpr8 :: MyParser GramExp
> pExpr8 = do
>   id <- pId
>   opt <- optionMaybe pOpt
>   case opt of
>     Nothing -> return $ GramExpId (Var id []) 
>     Just o -> do
>     case o of	
>       Left args -> return $ GramExpFunCall (GramFunCall id args) 
>       Right field -> return $ GramExpId (Var id [field]) 
>   where 
>     pOpt = (Left) <$> pArgList <|> (Right) <$> pField

Used to assign high precedence for expressions inside parenthesis
Grammar: Exp9 = '(' Exp [ ',' Exp ] ')'

> pExpr9 :: MyParser GramExp
> pExpr9 = do
>   (_, p) <- isToken TokenOpenP
>   e1 <- pExpr
>   opt <- optionMaybe pOpt
>   void $ isToken TokenCloseP
>   case opt of
>     Nothing -> return e1
>     Just e2 -> return $ GramExpTuple p e1 e2
>   where
>     pOpt = do
>       void $ isToken TokenComma
>       e2 <- pExpr
>       return (e2)	

> pEmptyList :: MyParser GramExp
> pEmptyList = do
>   (_, p) <- isToken TokenOpenSquareB
>   void $ isToken TokenCloseSquareB
>   return $ GramEmptyList p

Since the grammar-defined fields are just TokenIds in the
lexer, we need to match them manually in the parser. This
method returns a Maybe constructor for TokenIds. If the 
id is one of the reserved ones, it returns a constructor 
and if not, returns Nothing (which should never occur in
a well formed program).

> constField :: PosToken -> Maybe ([GramField] -> GramField)
> constField (TokenId "hd", p)  = Just $ Head p
> constField (TokenId "tl", p)  = Just $ Tail p
> constField (TokenId "fst", p) = Just $ First p
> constField (TokenId "snd", p) = Just $ Second p
> constField _                  = Nothing 

The SPL has a limited, well-defined list of fields available:
hd (head), tl (tail), fst (first) and snd (second). Every 
field starts with a TokenPeriod and is followed by one of 
the defined fields. We make use of the constField function
above to easily construct GramField based on TokenIds. If the
field isn't one of the reserved ones, it throwns an error. 

> pField :: MyParser GramField
> pField = do
>   void $ isToken TokenPeriod
>   tId <- isToken $ TokenId ""
>   let maybeConst = constField tId in
>     case maybeConst of 
>       Just constructor -> do
>         opt <- optionMaybe pField
>         let field = maybeToList opt in
>           return $ constructor field
>       Nothing -> error "Invalid field"

This parser is self-explanatory.

> pStmt :: MyParser GramStmt
> pStmt = pIf <|> pWhile <|> pStmt1 <|> pReturn <|> pFuncVarDecl

> pFuncVarDecl :: MyParser GramStmt
> pFuncVarDecl = do
>   varD <- pVarDecl
>   return $ GramFunVarDecl varD

Here a common prefix problem arises because both attribution and
function call start with "id". So, we left refactor this rule by
creating a new rule Stmt1 that consumes an id and checks what comes
after it. It can either be a list of arguments (pArgList) or an 
attribution body (pAttribBody)

> pStmt1 :: MyParser GramStmt
> pStmt1 = do
>   (Id p i) <- pId
>   opt <- (Left) <$> pAttribBody <|> (Right) <$> pArgList
>   void $ isToken TokenEOL
>   case opt of
>     Left (field, expr) -> return $ GramAttr p (Var (Id p i) field) expr
>     Right args -> return $ GramStmtFunCall (GramFunCall (Id p i) args) 

An attribution body is anything that comes after the variable name:
         " [Field] '='' Expr ';'' "

> pAttribBody :: MyParser ([GramField], GramExp)
> pAttribBody = do
>   optField <- optionMaybe pField
>   void $ isToken TokenAttribution
>   expr <- pExpr
>   let field = maybeToList optField in
>     return (field, expr)

Grammar: 'if' '(' Exp ')' '{' Stmt* '}' [ 'else' '{' Stmt* '}' ]

> pIf :: MyParser GramStmt
> pIf = do
>   (_, p) <- isToken TokenIf
>   void $ isToken TokenOpenP
>   condition <- pExpr
>   void $ isToken TokenCloseP
>   void $ isToken TokenOpenCurlyB
>   stmtsIf <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   optElse <- optionMaybe pElse
>   case optElse of
>     Nothing -> return $ GramIf p condition stmtsIf []
>     Just stmstsElse -> return $ GramIf p condition stmtsIf stmstsElse

pElse is used just internally in pIf because the ELSE part is 
optional.

> pElse :: MyParser [GramStmt]
> pElse = do
>   void $ isToken TokenElse
>   void $ isToken TokenOpenCurlyB
>   stmts <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   return stmts

Grammar: 'while' '(' Exp ')' '{' Stmt* '}'

> pWhile :: MyParser GramStmt
> pWhile = do
>   (_, p) <- isToken TokenWhile
>   void $ isToken TokenOpenP
>   condition <- pExpr
>   void $ isToken TokenCloseP 	
>   void $ isToken TokenOpenCurlyB
>   stmts <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   return $ GramWhile p condition stmts 

Grammar: 'return' [ Exp ] ';'

> pReturn :: MyParser GramStmt
> pReturn = do
>   (_, p) <- isToken TokenReturn
>   opt <- optionMaybe pExpr
>   void $ isToken TokenEOL
>   return $ GramReturn p opt

The parser pVarDecl is not used in Decl. It's used inside FuncDeclTail 
in the grammar rule that says that a function may have many variables
declared inside it.

> pVarDecl :: MyParser GramVarDecl
> pVarDecl = pVarDeclVar <|> pVarDeclCustom

The parser pVarDeclCustom is only used by pVarDecl, it's not used in 
pDecl. Check comments above.

> pVarDeclCustom :: MyParser GramVarDecl
> pVarDeclCustom = do
>   declType <- pType
>   declTail <- pVarDeclTail
>   return $ GramVarDeclType declType declTail

The parser pVarDeclVar is only used by pVarDecl, it's not used in pDecl. 
Chech comments above.

> pVarDeclVar :: MyParser GramVarDecl
> pVarDeclVar = do
>   void $ isToken TokenVar
>   declTail <- pVarDeclTail
>   return $ GramVarDeclVar declTail 

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
> getTypeId _ = error "getTypeId is supposed to work ony on GramTypeId"

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
>   declTail <- (Left) <$> pVarDeclTail <|> (Right) <$> pFuncDeclTail	
>   case declTail of
>     Left v -> return $ GramDeclVar (GramVarDeclType declType v) 
>     Right f -> return $ GramDeclFun (GramFuncDecl (getTypeId declType) f)

pDecl2 parses VarDecl. Since the TokenVar should've been parsed before
this is called and that it's useless for the Syntax Tree, we don't need it.
pDecl2 doesn't exis in the grammar, it's defined just so we can construct 
the corrent GramDecl with it.

> pDecl2 :: MyParser GramDecl
> pDecl2 = do
>   declTail <- pVarDeclTail
>   return $ GramDeclVar (GramVarDeclVar declTail)

> pVarDeclTail :: MyParser GramVarDeclTail
> pVarDeclTail = do
>   (Id i p) <- pId
>   void $ isToken TokenAttribution
>   expr <- pExpr
>   void $ isToken TokenEOL
>   return $ GramVarDeclTail (Id i p) expr 

> pFuncDeclTail :: MyParser GramFuncDeclTail
> pFuncDeclTail = do
>   void $ isToken TokenOpenP
>   optFArgs <- optionMaybe pFArgs
>   void $ isToken TokenCloseP
>   optFunType <- optionMaybe pOptType
>   void $ isToken TokenOpenCurlyB
>   stmts <- many pStmt
>   void $ isToken TokenCloseCurlyB
>   let args = maybeToList optFArgs
>       fType = maybeToList optFunType in 
>       return $ GramFuncDeclTail args fType stmts
>   where
>     pOptType = do
>       void $ isToken TokenFuncDecl
>       t <- pFunType
>       return t

This is the root parser and is the one that should be used to parse a 
full program.
Grammar: SPL = Decl+

> pSPL :: MyParser Gram
> pSPL = many1 pDecl























