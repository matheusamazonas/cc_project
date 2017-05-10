> module Generator where

> import Control.Monad (mapM_,sequence_)
> import Control.Monad.State
> import Control.Monad.Writer
> import Grammar
> import Token
> import Data.Char (ord)
> import Data.List (genericLength)


> type Depth = Integer
> type Id = String
> type Code = String
> type Scope = [(Id, Depth)]
> type EnvType = (Depth, [Scope], Int)

> type Environment = WriterT Code (State EnvType)



Code generation

Call with, e.g., run generateStmtBlock stmts, with stmts :: [GramStmt]
Once implemented, generate = run generateProgram

> generate :: Gram -> Code
> generate = run generateProgram 

> generateProgram :: Gram -> Environment ()
> generateProgram g = do 
>   write "bra main"
>   generateGram g
>   sequence_ builtins


> generateGram :: Gram -> Environment ()
> generateGram [] = return ()
> generateGram (x:xs) = case x of
>   GramDeclVar vardecl -> case vardecl of
>     GramVarDeclType t (GramVarDeclTail id@(Id p i) e) -> do -- type checker ensures only typed variable declarations
>       write $ "; define " ++ i
>       generateExpr e -- TODO global variable declarations
>       generateGram xs
>   GramDeclFun fundecl -> do
>     generateFunDecl fundecl
>     generateGram xs

> builtins = let und = undefined in
>   [generatePrint (GramListType und und), generatePrint (GramTupleType und und und), 
>    generatePrint (GramBasicType und IntType), generatePrint (GramBasicType und BoolType),
>    generatePrint (GramBasicType und CharType), runTimeException "__exc_untyped_variable" "Runtime exception: could not resolve overloading for print"]

> addArgs :: [GramId] -> Environment ()
> addArgs args = do
>   let c = genericLength args
>   addArgs' args (-2) -- c == MP and c-1 holds the return address. Hence c-2
>   where
>     addArgs' [] _ = do return ()
>     addArgs' ((Id _ argId):as) argC = do
>       addArg argId argC
>       addArgs' as (argC-1)

> getFuncReturnType :: [GramFunType] -> GramRetType
> getFuncReturnType ((GramFunType _ ret):_) = ret

> generateFunDecl :: GramFuncDecl -> Environment ()
> generateFunDecl (GramFuncDecl (Id _ funId) (GramFuncDeclTail args types stmts)) = do
>   pushScope
>   label funId
>   let argCounter = length args
>   if funId /= "main" then write $ "link 0" else return ()
>   addArgs args
>   generateStmtBlock stmts
>   case getFuncReturnType types of
>     (GramVoidType _) -> if funId /= "main" then write "unlink\nret" else return ()
>     otherwise -> do return ()
>   if funId == "main" then do
>     write "trap 0"
>     write "halt"
>   else do return ()

> generateFunCall :: GramFunCall -> Environment ()
> generateFunCall (GramOverloadedFunCall _ (Id _ funId) args) = do
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   write $ "bsr " ++ show funId
> generateFunCall (GramFunCall (Id _ funId) rev_args) = do
>   sequence $ map generateExpr rev_args
>   write $ "bsr " ++ show funId

> generateStmtBlock :: [GramStmt] -> Environment ()
> generateStmtBlock stmts = do
>   pushScope
>   generateStmtBlock' stmts
>   where generateStmtBlock' [] = popScope
>         generateStmtBlock' (stmt:stmts) = do
>           generateStmt stmt
>           generateStmtBlock' stmts

> generateStmt :: GramStmt -> Environment ()
> generateStmt (GramWhile _ expr stmts) = do
>   wStart <- genLabel "while_start"
>   wEnd <- genLabel "while_end"
>   label wStart
>   generateExpr expr
>   write $ "brf " ++ wEnd
>   generateStmtBlock stmts
>   write $ "bra " ++ wStart
>   label wEnd
> generateStmt (GramIf _ expr thenStmts elseStmts) = do
>   labelElse <- genLabel "else"
>   labelIf <- genLabel "if"
>   labelFi <- genLabel "fi"
>   generateExpr expr
>   write $ "brf " ++ labelElse
>   generateStmtBlock thenStmts 
>   write $ "bra " ++ labelFi
>   label labelElse 
>   generateStmtBlock elseStmts
>   label labelFi
> generateStmt (GramReturn _ (Nothing)) = write "unlink\nret"
> generateStmt (GramReturn _ (Just expr)) = do
>   generateExpr expr
>   write "str RR\nunlink\nsts -1\nret"
> generateStmt (GramFunVarDecl (GramVarDeclType t (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr
> generateStmt (GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr
> generateStmt (GramAttr _ (Var (Id _ varId) fields) expr) = do
>   varLoc <- lookupVar varId
>   generateExpr expr
>   write $ "stl " ++ show varLoc


> generateExpr :: GramExp -> Environment ()
> generateExpr (GramBool _ True) = write "ldc -1"
> generateExpr (GramBool _ False) = write "ldc 0"
> generateExpr (GramChar _ char) = write $ "ldc " ++ show (ord char)
> generateExpr (GramNum _ i) = write $ "ldc " ++ show i
> generateExpr (GramEmptyList _) = write "ldc 0" -- null pointer
> generateExpr (GramExpTuple _ e1 e2) = do
>   generateExpr e1 
>   generateExpr e2 
>   write "stmh 2"
> generateExpr (GramBinary _ op expr1 expr2) = do
>   generateExpr expr1
>   generateExpr expr2
>   write $ generateBinaryOperation op
> generateExpr (GramOverloadedBinary _ t op expr1 expr2) = do
>   generateExpr expr1
>   generateExpr expr2
>   generateOverloadedOperation t op
> generateExpr (GramUnary _ op expr) = do
>   generateExpr expr
>   write $ generateUnaryOperation op
> generateExpr (GramExpId (Var (Id _ varId) fields)) = do
>   varLoc <- lookupVar varId
>   write $ "ldl " ++ show varLoc
> generateExpr (GramExpFunCall funCall) = do
>   generateFunCall funCall
>   write "ldr RR"

> generateBinaryOperation :: Operation -> String
> generateBinaryOperation Minus           = "sub"
> generateBinaryOperation Plus            = "add"
> generateBinaryOperation Times           = "mul"
> generateBinaryOperation Division        = "div"
> generateBinaryOperation Mod             = "mod"
> generateBinaryOperation LogicalOr       = "or"
> generateBinaryOperation LogicalAnd      = "and"
> generateBinaryOperation LessThan        = "lt"
> generateBinaryOperation LessOrEqual     = "le"
> generateBinaryOperation GreaterThan     = "gt"
> generateBinaryOperation GreaterOrEqual  = "ge"
> generateBinaryOperation ListConst       = "stmh 2"

> generateUnaryOperation :: Operation -> String
> generateUnaryOperation Minus            = "neg"
> generateUnaryOperation LogicalNot       = "not"

> generateOverloadedOperation :: GramType -> Operation -> Environment () -- environment used for type variable bindings later
> generateOverloadedOperation t op = do
>   lab  <- genLabel "overl_eq"
>   write $ "bra " ++ lab
>   call <- equals t
>   label lab
>   case op of
>     Equals    -> write call
>     Different -> do
>       write call
>       write "not"
>   where equals :: GramType -> Environment String
>         equals (GramBasicType _ BoolType) = return "eq"
>         equals (GramBasicType _ CharType) = return "eq"
>         equals (GramBasicType _ IntType)  = return "eq"
>         equals (GramTupleType _ t1 t2) = do
>           teq  <- genLabel "tuple_eq"
>           teqp <- genLabel "tuple_eq_post"
>           call1 <- equals t1 
>           call2 <- equals t2
>           
>           write $ teq ++ ": lds -2\nldh -1\nlds -2\nldh -1" -- load left elements
>           write call1
>           write $ "lds 0\nbrf " ++ teqp -- shortcut in case of non-equality
>           write "lds -3\nldh 0\nlds -3\nldh 0" -- load right elements
>           write call2
>           write "and"
>           write $ teqp ++ ": str RR\nret" -- pop all but True/False result
>           return $ "bsr " ++ teq ++ "\najs -2\nldr RR"
>         equals (GramListType _ t) = do
>           leq     <- genLabel "list_eq"
>           leqn1   <- genLabel "list_eq_null_1"
>           leqn2   <- genLabel "list_eq_null_2"
>           leqp    <- genLabel "list_eq_post"
>           innercall <- equals t 
>           
>           write $ leq ++ ": lds -2\nlds -2\nbrf " ++ leqn1 ++ "\nbrf " ++ leqn2 -- check for null lists (base case)
>           write $ "lds -2\nldh 0\nlds -2\nldh 0\nbsr " ++ leq ++ "\najs -2" -- recurse through tails
>           write $ "ldr RR\nbrf " ++ leqp -- shortcut in case of non-equality
>           write $ "lds -2\nldh -1\nlds -2\nldh -1" -- load heads
>           write innercall -- inner call
>           write $ "str RR\nret"
>           write $ leqn2 ++ ": ldc 0\nstr RR\nbra " ++ leqp -- first list empty - return isEmpty(2nd list)
>           write $ leqn1 ++ ": ldc 0\neq\nstr RR" -- first list non-empty, second list empty - return false
>           write $ leqp ++ ": ret"
>           return $ "bsr " ++ leq ++ "\najs -2\nldr RR"
>         equals (GramIdType id) = return "halt" -- TODO look up type vars for overloading




Overloading handlers


A type frame is generated during a print call.
This contains information about which print methods should be used during execution.
In particular, types containing types (e.g., lists and tuples) use this information
to determine at runtime which print functions should be called for their elements.

A type frame is a tuple defined as follows.
Here, TF_element, TF_fst and TF_snd are type frames for the contained types.
int/char/bool: TF = (__print_char/int/bool, null)
list: TF = (__print_list, TF_element)
tuple: TF = (__print_tuple, (TF_fst, TF_snd))

> callPrint :: GramType -> Environment ()
> callPrint (GramBasicType _ CharType) = write "bsr __print_char"
> callPrint (GramBasicType _ IntType)  = write "bsr __print_int"
> callPrint (GramBasicType _ BoolType) = write "bsr __print_bool"
> callPrint (GramListType _ t) = do
>   printFrame t
>   write "lds -1\nbsr __print_list"
> callPrint (GramTupleType _ t1 t2) = do
>   printFrame t1
>   printFrame t2
>   write "stmh 2\nlds -1\nbsr __print_tuple"
> callPrint (GramIdType _) = write "bsr __exc_untyped_variable"

> printFrame :: GramType -> Environment ()
> printFrame (GramBasicType _ CharType) = write "ldc __print_char\nldc 0\nstmh 2"
> printFrame (GramBasicType _ IntType)  = write "ldc __print_int\nldc 0\nstmh 2"
> printFrame (GramBasicType _ BoolType) = write "ldc __print_bool\nldc 0\nstmh 2"
> printFrame (GramListType _ t) = do
>   write "ldc __print_list"
>   printFrame t
>   write "stmh 2"
> printFrame (GramTupleType _ t1 t2) = do
>   write "ldc __print_tuple"
>   printFrame t1
>   printFrame t2
>   write "stmh 2\nstmh 2"
> printFrame (GramIdType _) = write "ldc __exc_untyped_variable\nldc 0\nstmh 2"

> generatePrint :: GramType -> Environment ()
> generatePrint (GramBasicType _ CharType) = write "__print_char: lds -1\ntrap 1\nret"
> generatePrint (GramBasicType _ IntType)  = write "__print_int: lds -1\ntrap 0\nret"
> generatePrint (GramBasicType _ BoolType) = do
>   write "__print_bool: lds -1\nbrf __print_bool_false"
>   printText "True"
>   write "ret"
>   label "__print_bool_false"
>   printText "False"
>   write "ret"
> generatePrint (GramListType _ _) = do
>   label "__print_list"
>   printChar '['
>   write "lds -1\nbrf print_list_post" -- check for empty list
>   write "print_list_elem: lds -2\nldh 0" -- load inner type frame
>   write "\nlds -2\nldh -1" -- load head
>   write "\nlds -4\nldh -1\njsr\najs -2" -- print head 
>   write "lds -1\nldh 0\nbrf print_list_post" -- close singleton list
>   printText ", "
>   write "lds -1\nldh 0\nsts -2\nbra print_list_elem" -- for longer lists, recurse
>   label "print_list_post"
>   printChar ']'
>   write "ret"
> generatePrint (GramTupleType _ _ _) = do
>   label "__print_tuple"
>   printChar '('
>   write "lds -2\nldh -1\nlds 0\nldh 0" -- load left type frame
>   write "lds -3\nldh -1" -- load left element
>   write "lds -2\nldh -1\njsr\najs -3" -- print left element
>   printText ", "
>   write "lds -2\nldh 0\nlds 0\nldh 0" -- load right type frame
>   write "lds -3\nldh 0" -- load right element
>   write "lds -2\nldh -1\njsr\najs -3" -- print right element
>   printChar ')'
>   write "ret"   





Exception handlers

> runTimeException :: String -> String -> Environment ()
> runTimeException lab s = do
>   write $ "; " ++ s
>   label lab
>   printChar '\n'
>   printText s
>   printChar '\n'
>   write "halt\n"

> printText :: String -> Environment ()
> printText = mapM_ printChar

> printChar :: Char -> Environment ()
> printChar c = write $ "ldc " ++ show (ord c) ++ "\ntrap 1"



Scope handlers

> pushScope :: Environment ()
> pushScope = do
>   (d, ss, i) <- get
>   put (d, []:ss, i)

> popScope :: Environment ()
> popScope = do
>   (d, s:ss, i) <- get
>   put (d, ss, i)



Variable handlers

> addVar :: Id -> Environment ()
> addVar id = do
>   (d, (s:ss), i) <- get
>   put (d+1, ((id, d):s):ss, i)

> addArg :: Id -> Integer -> Environment ()
> addArg varId id = do
>   (d, (s:ss), i) <- get
>   put (d, ((varId, id):s):ss, i)

> lookupVar :: Id -> Environment Depth
> lookupVar varId = do
>   (_, ss, _) <- get
>   return $ lookupVar' varId ss
>   where lookupVar' _ [] = -999
>         lookupVar' varId (s:ss) =
>           case lookup varId s of
>             Nothing -> lookupVar' varId ss
>             Just d -> d


Label handlers

> genLabel :: String -> Environment String
> genLabel s = do
>   (d, ss, i) <- get
>   put (d, ss, i+1)
>   return $ s ++ "_" ++ show i


Environment handlers

> run :: (t -> Environment ()) -> t -> Code
> run gen gram = evalState (execWriterT (gen gram)) initEnv

> label :: String -> Environment ()
> label l = tell (l ++ ": ")

> write :: String -> Environment ()
> write c = tell (c ++ "\n")

> initEnv :: EnvType
> initEnv = (1, [[]], 1)
