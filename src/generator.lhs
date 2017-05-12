> module Generator where

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Writer (WriterT, tell, execWriterT)
> import Data.Char (ord)
> import Data.List (genericLength, isPrefixOf)
> import Grammar
> import Token


> type Depth = Integer
> type Id = String
> type Code = String
> type Scope = (Depth, [(Id, (Code, Code))])
> type EnvType = ([Scope], Int)

> type Environment = WriterT Code (State EnvType)



Code generation

Call with, e.g., run generateStmtBlock stmts, with stmts :: [GramStmt]
Once implemented, generate = run generateProgram

> generate :: Gram -> Code
> generate = postprocess . run generateProgram 

> generateProgram :: Gram -> Environment ()
> generateProgram g = do 
>   write "bra __init"
>   let (globals, funcs) = sepDecls g
>   sequence $ map addGlobal $ map getVarId globals
>   sequence $ replicate (length globals) $ write "nop"
>   sequence $ map generateFunDecl funcs
>   write "\n; initialise global variables"
>   label "__init"
>   sequence $ map generateVariable globals
>   write "bra main"
>   sequence_ builtins

> sepDecls :: [GramDecl] -> ([GramVarDecl], [GramFuncDecl])
> sepDecls [] = ([], [])
> sepDecls ((GramDeclVar d):ds) = (d:vars, funcs)
>   where
>     (vars, funcs) = sepDecls ds
> sepDecls ((GramDeclFun d):ds) = (vars, d:funcs)
>   where
>     (vars, funcs) = sepDecls ds

> getVarId :: GramVarDecl -> String
> getVarId (GramVarDeclType _ (GramVarDeclTail (Id _ varId) _)) = varId
> getVarId (GramVarDeclVar (GramVarDeclTail (Id _ varId) _)) = varId

> generateVariable :: GramVarDecl -> Environment ()
> generateVariable (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr)) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr
>   (_, store) <- lookupVar varId
>   write store
> generateVariable (GramVarDeclType _ (GramVarDeclTail (Id _ varId) expr)) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr
>   (_, store) <- lookupVar varId
>   write store

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
>   write $ "\n; define function " ++ funId
>   pushScope
>   label funId
>   let argCounter = length args
>   if funId /= "main" then write "link 0" else write "nop"
>   addArgs args
>   generateStmtBlock stmts
>   case getFuncReturnType types of
>     (GramVoidType _) -> if funId /= "main" then write "unlink\nret" else return ()
>     otherwise -> do return ()
>   if funId == "main" then do
>     write "halt"
>   else do 
>     write "unlink\nret"

> genVarDecl :: GramVarDecl -> Environment ()
> genVarDecl (GramVarDeclType varType (GramVarDeclTail (Id _ varId) expr)) = do
>   addVar varId
>   generateExpr expr
> genVarDecl (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr)) = do
>   addVar varId
>   generateExpr expr

> generateFunCall :: GramFunCall -> Environment ()
> generateFunCall (GramOverloadedFunCall ts (Id _ funId) args) = do
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   if funId == "print" then callPrint $ head ts
>   else write $ "bsr " ++ funId
>   write $ "ajs " ++ show (-length args) 

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
>   write "str RR\nunlink\nret"
> generateStmt (GramFunVarDecl (GramVarDeclType t (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr
> generateStmt (GramFunVarDecl (GramVarDeclVar (GramVarDeclTail (Id _ varId) expr))) = do
>   addVar varId
>   generateExpr expr
> generateStmt (GramAttr _ (Var (Id _ varId) fields) expr) = do
>   (load, store) <- lookupVar varId
>   generateExpr expr
>   write load
>   descendFields True store fields
> generateStmt (GramStmtFunCall funCall) = do generateFunCall funCall


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
>   (load, store) <- lookupVar varId
>   write load
>   descendFields False store fields
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
> generateOverloadedOperation t op = case op of
>   Equals    -> callEquals t
>   Different -> do
>     callEquals t
>     write "not"

> descendFields :: Bool -> Code -> [GramField] -> Environment ()
> descendFields False _ [] = return ()
> descendFields True store [] = do
>   write "ajs -1"
>   write store
> descendFields save store [x] = case x of
>   First p fields  -> do
>     descend save store "-1" fields
>   Second p fields -> do
>     descend save store "0" fields
>   Head p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend save store "-1" fields
>   Tail p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend save store "0" fields
>   where descend :: Bool -> Code -> String -> [GramField] -> Environment ()
>         descend False store offset fields = do
>           write $ "ldh " ++ offset
>           descendFields False store fields
>         descend True store offset fields = case fields of
>           (f:fs) -> do
>             write $ "ldh " ++ offset
>             descendFields True store fields
>           [] -> write $ "sta " ++ offset


Overloading handlers

> callPrint :: GramType -> Environment ()
> callPrint (GramBasicType _ CharType) = write "bsr __print_char"
> callPrint (GramBasicType _ IntType)  = write "bsr __print_int"
> callPrint (GramBasicType _ BoolType) = write "bsr __print_bool"
> callPrint (GramListType _ t) = do
>   typeFrame "print" t
>   write "lds -1\nbsr __print_list"
> callPrint (GramTupleType _ t1 t2) = do
>   typeFrame "print" t1
>   typeFrame "print" t2
>   write "stmh 2\nlds -1\nbsr __print_tuple"
> callPrint (GramIdType _) = write "bsr __exc_untyped_variable"

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


> callEquals :: GramType -> Environment ()
> callEquals (GramBasicType _ CharType) = write "eq"
> callEquals (GramBasicType _ IntType) = write "eq"
> callEquals (GramBasicType _ BoolType) = write "bsr __eq_bool\nldr RR"
> callEquals (GramListType _ t) = do
>   typeFrame "eq" t
>   write "lds -2\nlds -2\nbsr __eq_list\najs -5\nldr RR"
> callEquals (GramTupleType _ t1 t2) = do
>   typeFrame "eq" t1
>   typeFrame "eq" t2
>   write "stmh 2\nlds -2\nlds -2\nbsr __eq_tuple\najs -5\nldr RR"
> callEquals (GramIdType _) = write "bsr __exc_untyped_variable"


> generateEquals :: GramType -> Environment ()
> generateEquals (GramBasicType _ CharType) = write "__eq_char: lds -2\nlds -2\neq\nstr RR\nret"
> generateEquals (GramBasicType _ IntType)  = write "__eq_int: lds -2\nlds -2\neq\nstr RR\nret"
> generateEquals (GramBasicType _ BoolType) = do
>   write "__eq_bool: lds -2\nldc 0\nne\nlds -2\nldc 0\nne" -- convert to "unequal to zero" representation as -1 and 1 are both used by SSM
>   write "eq\nstr RR\nret"
> generateEquals (GramListType _ _) = do
>   write "__eq_list: lds -2\nlds -2\nbrf __eq_list_null1\nbrf __eq_list_retfalse" -- check for null lists (base case)
>   write "lds -3\nldh 0" -- load inner type frame
>   write "lds -3\nldh -1\nlds -3\nldh -1" -- load both heads
>   write "lds -6\nldh -1\njsr\najs -3\nldr RR\nbrf __eq_list_retfalse" -- compare heads
>   write "lds -3\nlds -3\nldh 0\nlds -3\nldh 0" -- copy type frame and list tails
>   write "bsr __eq_list\najs -3\nret" -- recursive call on tails
>   write "__eq_list_null1: ldc 0\neq\nbra __eq_list_post" -- first list empty - return isEmpty(second list)
>   write "__eq_list_retfalse: ldc 0" -- first list non-empty, second empty - return false
>   write "__eq_list_post: str RR\nret"
> generateEquals (GramTupleType _ _ _) = do
>   write "__eq_tuple: lds -3\nldh -1\nldh 0" -- load left element's type frame
>   write "lds -3\nldh -1\nlds -3\nldh -1" -- load both left elements
>   write "lds -6\nldh -1\nldh -1\njsr\najs -3\nldr RR\nbrf __eq_tuple_retfalse" -- compare left elements
>   write "lds -3\nldh 0\nldh 0" -- load right element's type frame
>   write "lds -3\nldh 0\nlds -3\nldh 0" -- load both right elements
>   write "lds -6\nldh 0\nldh -1\njsr\najs -3\nret" -- compare right elements
>   write "__eq_tuple_retfalse: ldc 0\nstr RR\nret"


Type frame handlers

A type frame is generated during an overloaded operation such as (==) or print.
This contains information about which print methods should be used during execution.
In particular, types containing types (e.g., lists and tuples) use this information
to determine at runtime which print functions should be called for their elements.

A type frame is a tuple defined as follows.
Here, TF_element, TF_fst and TF_snd are type frames for the contained types.
int/char/bool: TF = (__print_char/int/bool, null)
list: TF = (__print_list, TF_element)
tuple: TF = (__print_tuple, (TF_fst, TF_snd))

> typeFrame :: String -> GramType -> Environment ()
> typeFrame fname (GramBasicType _ CharType) = write $ "ldc __" ++ fname ++ "_char\nldc 0\nstmh 2"
> typeFrame fname (GramBasicType _ IntType)  = write $ "ldc __" ++ fname ++ "_int\nldc 0\nstmh 2"
> typeFrame fname (GramBasicType _ BoolType) = write $ "ldc __" ++ fname ++ "_bool\nldc 0\nstmh 2"
> typeFrame fname (GramListType _ t) = do
>   write $ "ldc __" ++ fname ++ "_list"
>   typeFrame fname t
>   write "stmh 2"
> typeFrame fname (GramTupleType _ t1 t2) = do
>   write $ "ldc __" ++ fname ++ "_tuple"
>   typeFrame fname t1
>   typeFrame fname t2
>   write "stmh 2\nstmh 2"
> typeFrame _ (GramIdType _) = write "ldc __exc_untyped_variable\nldc 0\nstmh 2"



Standard library handlers

> builtins = let und = undefined in
>   [polyBuildIn "print" generatePrint,
>    polyBuildIn "==" generateEquals,
>    generateIsEmpty,
>    runTimeException "__exc_empty_list_traversal" "Runtime exception: empty list traversed",
>    runTimeException "__exc_untyped_variable" "Runtime exception: could not resolve overloading for print"]

> generateIsEmpty :: Environment ()
> generateIsEmpty = do
>   write "\n;define isEmpty"
>   write "isEmpty: lds -1\nldc 0\neq\nstr RR\nret"

> polyBuildIn :: String -> (GramType -> Environment ()) -> Environment ()
> polyBuildIn s f = do
>   let und = undefined
>   let types = [GramBasicType und CharType, GramBasicType und IntType, GramBasicType und BoolType, 
>                GramListType und und, GramTupleType und und und]
>   write $ "\n; define polymorphic " ++ s
>   mapM_ f types


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


Post-processing

> postprocess :: Code -> Code
> postprocess = fixDoubleLabels

> fixDoubleLabels :: Code -> Code
> fixDoubleLabels code = 
>   let fixes = map (findDoubleLabels . words) $ lines code in
>   let substitute = applySub $ foldr (++) [] $ map snd fixes in
>   unlines $ map (substitute . unwords . fst) fixes
>   where findDoubleLabels wrds
>           | length wrds > 2 && last (head wrds) == ':' && last (head $ tail wrds) == ':' = 
>               (tail wrds, [(init $ head wrds, init $ head $ tail wrds)])
>           | otherwise = (wrds, [])
>         applySub subs ln
>           | any (\i -> isPrefixOf i ln) subbedInstructions = 
>             case lookup (drop 4 ln) subs of
>               Just lab -> (take 4 ln) ++ lab
>               Nothing  -> ln
>           | otherwise = ln
>         subbedInstructions = ["bra ", "brf ", "brt ", "bsr ", "ldc "]





Scope handlers

> pushScope :: Environment ()
> pushScope = do
>   (ss, i) <- get
>   put ((1,[]):ss, i)

> popScope :: Environment ()
> popScope = do
>   (s:ss, i) <- get
>   put (ss, i)



Variable handlers

> addVar :: Id -> Environment ()
> addVar id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldl " ++ show d
>       storeIns = "stl " ++ show d
>   put ((d+1,(id, (loadIns, storeIns)):v):ss, i)

> addGlobal :: Id -> Environment ()
> addGlobal id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldc " ++ show (d+1) ++ "\nlda 0"
>       storeIns = "ldc " ++ show (d+1) ++ "\nsta 0"
>   put ((d+1,(id, (loadIns, storeIns)):v):ss, i)

> addArg :: Id -> Integer -> Environment ()
> addArg varId id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldl " ++ show id
>       storeIns = "stl " ++ show id
>   put ((d,(varId, (loadIns, storeIns)):v):ss, i)

> lookupVar :: Id -> Environment (Code, Code)
> lookupVar varId = do
>   (ss, _) <- get
>   return $ lookupVar' varId ss
>   where lookupVar' _ [] = ("ldl " ++ show (-999), "stl " ++ show(-999))
>         lookupVar' varId ((_,v):vs) =
>           case lookup varId v of
>             Nothing -> lookupVar' varId vs
>             Just d -> d


Label handlers

> genLabel :: String -> Environment String
> genLabel s = do
>   (ss, i) <- get
>   put (ss, i+1)
>   return $ s ++ "_" ++ show i


Environment handlers

> run :: (t -> Environment ()) -> t -> Code
> run gen gram = evalState (execWriterT (gen gram)) initEnv

> label :: String -> Environment ()
> label l = tell (l ++ ": ")

> write :: String -> Environment ()
> write c = tell (c ++ "\n")

> initEnv :: EnvType
> initEnv = ([(1,[])], 1)
