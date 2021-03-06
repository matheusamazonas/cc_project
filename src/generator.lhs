> module Generator where

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Writer (WriterT, tell, execWriterT)
> import Data.Char (ord, digitToInt)
> import Data.List ((!!), find, genericLength, intersect, isPrefixOf, nub, (\\), isSuffixOf)
> import Data.List.Utils (replace)
> import Text.Parsec.Pos (sourceLine, sourceColumn)
> import Dependency (Capture(..), freeTypeVars)
> import Grammar
> import Token
> import Error

> type Depth = Integer
> type Id = String
> type Code = String
> type Scope = (Depth, [(Id, (Code, Code, Code))])
> type EnvType = ([Scope], Int)

> type Environment = WriterT Code (State EnvType)



Code generation

Call with, e.g., run generateStmtBlock stmts, with stmts :: [GramStmt]
Once implemented, generate = run generateProgram

> generate :: [Capture] -> Gram -> Either CompilationError Code
> generate capts g
>   | hasMain g = pure $ postprocess $ run (generateProgram capts) g
>   | otherwise = Left $ CompilationError CodeGeneration ("Program doesn't contain a void-returning function main()") undefined

> generateProgram :: [Capture] -> Gram -> Environment ()
> generateProgram capts g = do 
>   write "bra __init"
>   let (globals, funcs) = sepDecls g
>   sequence_ $ map addGlobalFunc builtinNames
>   sequence_ $ map (addGlobal . getVarId . GramDeclVar) globals
>   sequence_ $ map (addGlobalFunc . getVarId . GramDeclFun) funcs
>   sequence_ $ map (generateFunDecl capts) funcs
>   write "\n; initialise global variables"
>   label "__init"
>   sequence_ $ map generateVariable globals
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

> hasMain :: Gram -> Bool
> hasMain [] = False
> hasMain (GramDeclFun (GramFuncDecl (Id _ "main") []  [GramFunTypeAnnot [] (GramVoidType _)] _) : fs) = True
> hasMain (_:ds) = hasMain ds

> getVarId :: GramDecl -> String
> getVarId (GramDeclVar (GramVarDeclType _ (Id _ varId) _)) = varId
> getVarId (GramDeclVar (GramVarDeclVar (Id _ varId) _)) = varId
> getVarId (GramDeclFun (GramFuncDecl (Id _ funId) _ _ _)) = funId

> generateVariable :: GramVarDecl -> Environment ()
> generateVariable (GramVarDeclVar (Id _ varId) expr) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr
> generateVariable (GramVarDeclType _ (Id _ varId) expr) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr

> addArgs :: [GramId] -> Environment ()
> addArgs args = do
>   addArgs' args (-2) -- c == MP and c-1 holds the return address. Hence c-2
>   where
>     addArgs' [] _ = do return ()
>     addArgs' ((Id _ argId):args) argC = do
>       addArg argId argC
>       addArgs' args (argC-1)

> getFuncReturnType :: [GramFunTypeAnnot] -> GramRetType
> getFuncReturnType ((GramFunTypeAnnot _ ret):_) = ret

> generateFunDecl :: [Capture] -> GramFuncDecl -> Environment ()
> generateFunDecl capts (GramFuncDecl id@(Id _ funId) args types stmts) = do
>   let capt = getCapture capts id
>   write $ "\n\n; define function " ++ funId
>   pushScope
>   label funId
>   let argCounter = length args
>   if funId /= "main" then write "link 0" else return ()
>   let tvarids = getArgTypes types
>   numTFArgs <- addTypeFrameArgs argCounter tvarids
>   mapM_ (captureIfNeeded capt) $ freeTypeIds tvarids
>   addArg "__env" $ toInteger $ -(2 + argCounter + numTFArgs)
>   addFunctionEnvironment capt
>   addArgs args
>   mapM_ (captureIfNeeded capt) args
>   pushScope
>   lams <- generateStmtBlock capt stmts
>   case getFuncReturnType types of
>     (GramVoidType _) -> if funId == "main" then write "halt" else write "unlink\nret"
>     otherwise -> do return ()
>   sequence_ lams
>   popScope
>   popScope
>   where getArgTypes [GramFunTypeAnnot ftypes tret] = 
>           case tret of (GramVoidType _) -> ftypes
>                        (GramRetType  t) -> t:ftypes

> generateFunCall :: GramFunCall -> Environment ()
> generateFunCall (GramOverloadedFunCall ts (Id _ funId) args) = do
>   (load, _, _) <- lookupVar funId
>   write $ fixHeapFunctionCalls $ load ++ "\nldh 0" -- load environment
>   mapM_ typeFrame $ reverse ts
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   write $ collapseConstantBranches $ (fixHeapFunctionCalls $ load ++ "\nldh -1") ++ "\njsr" -- actual function label
>   write $ "ajs " ++ show (-1 - length args - length ts) 
> generateFunCall (GramFunCall (Id _ funId) args) = do
>   (load, _, _) <- lookupVar funId
>   write $ fixHeapFunctionCalls $ load ++ "\nldh 0" -- load environment
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   write $ collapseConstantBranches $ (fixHeapFunctionCalls $ load ++ "\nldh -1") ++ "\njsr" -- actual function label
>   write $ "ajs " ++ show (-1 - length args) 

> generateStmtBlock :: Capture -> [GramStmt] -> Environment [Environment ()]
> generateStmtBlock capt [] = return []
> generateStmtBlock capt (stmt:stmts) = do
>   lam <- generateStmt capt stmt
>   lams <- generateStmtBlock capt stmts
>   return $ lam ++ lams

> generateStmt :: Capture -> GramStmt -> Environment [Environment ()]
> generateStmt capt (GramWhile _ expr stmts) = do
>   wStart <- genLabel "while_start"
>   wEnd <- genLabel "while_end"
>   label wStart
>   generateExpr expr
>   write $ "brf " ++ wEnd
>   pushScope
>   lams <- generateStmtBlock capt stmts
>   popScope
>   write $ "bra " ++ wStart
>   label wEnd
>   return lams
> generateStmt capt (GramIf _ expr thenStmts elseStmts) = do
>   labelElse <- genLabel "else"
>   labelIf <- genLabel "if"
>   labelFi <- genLabel "fi"
>   generateExpr expr
>   write $ "brf " ++ labelElse
>   pushScope
>   trlams <- generateStmtBlock capt thenStmts 
>   popScope
>   write $ "bra " ++ labelFi
>   label labelElse 
>   pushScope
>   falams <- generateStmtBlock capt elseStmts
>   popScope
>   label labelFi
>   return $ trlams ++ falams
> generateStmt (Capture (Id _ funId) _ _) (GramReturn _ (Nothing)) = do
>   if funId == "main" then write "halt" else write "unlink\nret"
>   return []
> generateStmt capt (GramReturn _ (Just expr)) = do
>   generateExpr expr
>   write "str RR\nunlink\nret"
>   return []
> generateStmt capt (GramFunVarDecl (GramVarDeclType t id@(Id _ varId) expr)) = do
>   addVar varId
>   generateExpr expr
>   captureIfNeeded capt id
>   return []
> generateStmt capt (GramFunVarDecl (GramVarDeclVar id@(Id _ varId) expr)) = do
>   addVar varId
>   generateExpr expr
>   captureIfNeeded capt id
>   return []
> generateStmt capt (GramAttr _ (Var (Id _ varId) fields) expr) = do
>   (load, store, _) <- lookupVar varId
>   generateExpr expr
>   case fields of
>     [] -> write store -- write new reference
>     (f:_) -> do
>       write load      -- assign to fields by reference
>       descendFields True f
>   return []
> generateStmt capt (GramStmtFunCall funCall) = do
>   generateFunCall funCall
>   return []
> generateStmt (Capture _ _ nestedcapts) (GramStmtFuncDecl (GramFuncDecl fid@(Id p funId) args annot stmts)) = do
>   write $ "ldc " ++ lambdaName
>   functionEnvironment nestedcapts fid
>   write "stmh 2"
>   addVar funId
>   return [generateFunDecl lambdaCapts lambdaDecl]
>   where lambdaDecl = GramFuncDecl (Id p lambdaName) args annot stmts
>         lambdaName = "_lambda_l" ++ (show $ sourceLine p) ++ "c" ++ (show $ sourceColumn p) ++ "_" ++ funId
>         (Capture (Id np _) ncaptured nnestedcapts) = getCapture nestedcapts fid
>         lambdaCapts = [Capture (Id np lambdaName) ncaptured nnestedcapts]


> generateExpr :: GramExp -> Environment ()
> generateExpr (GramBool _ True) = write "ldc -1"
> generateExpr (GramBool _ False) = write "ldc 0"
> generateExpr (GramChar _ char) = write $ "ldc " ++ show (ord char) ++ " ; character " ++ show char
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
>   (load, store, _) <- lookupVar varId
>   write load
>   case fields of
>     [] -> return ()
>     (f:_) -> descendFields False f
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

> descendFields :: Bool -> GramField -> Environment ()
> descendFields store f = case f of
>   First p fields  -> do
>     descend store "-1" fields
>   Second p fields -> do
>     descend store "0" fields
>   Head p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend store "-1" fields
>   Tail p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend store "0" fields
>   where descend :: Bool -> String -> [GramField] -> Environment ()
>         descend False offset fields = do
>           write $ "ldh " ++ offset
>           case fields of
>             [] -> return ()
>             (f:_) -> descendFields False f
>         descend True offset fields = 
>           case fields of
>             [] -> write $ "sta " ++ offset
>             (f:_) -> do
>               write $ "ldh " ++ offset
>               descendFields True f


Type frame handlers

A type frame is generated during an overloaded operation such as (==) or print.
This contains type information so that it can be decided which print/equals methods 
should be used, dynamically, during execution. In particular, types containing types 
(e.g., lists and tuples) use this information to determine at runtime 
which print/equals functions should be called for their elements.

A type frame is a tuple defined as follows.
Here, TF_element, TF_fst and TF_snd are type frames for the contained types.
_char, _list, etc. are offsets used by print/equals to find the right implementation.
int/char/bool: TF = (_char/_int/_bool, null)
list: TF = (_list, TF_element)
tuple: TF = (_tuple, (TF_fst, TF_snd))

> typeFrame :: GramType -> Environment ()
> typeFrame (GramBasicType _ CharType) = write "ldc 3 ; char type frame\nldc 0\nstmh 2"
> typeFrame (GramBasicType _ IntType)  = write "ldc 5 ; int type frame\nldc 0\nstmh 2"
> typeFrame (GramBasicType _ BoolType) = write "ldc 7 ; bool type frame\nldc 0\nstmh 2"
> typeFrame (GramListType _ t) = do
>   write "ldc 9 ; list type frame"
>   typeFrame t
>   write "stmh 2"
> typeFrame (GramTupleType _ t1 t2) = do
>   write "ldc 11 ; tuple type frame"
>   typeFrame t1
>   typeFrame t2
>   write "stmh 2\nstmh 2"
> typeFrame (GramIdType (Id _ id))
>   | isPrefixOf "_t" id || isPrefixOf "_v" id = do
>     (load, _, _) <- lookupVar $ "__tf_" ++ drop 2 id
>     write $ repl load 
>   | otherwise = write "bra __exc_unknown_error"
>   where repl "bra __exc_unknown_error" = "ldc 13\nldc 0\nstmh 2" -- catches error in case of e.g. empty list of type [_v1]
>         repl ld = ld                                             -- and replaces with more specific runtime exception if faulty
> typeFrame _ = write "ldc 15 ; function type frame\nldc 0\nstmh 2"

> addTypeFrameArgs :: Int -> [GramType] -> Environment Int
> addTypeFrameArgs numArgs ts = do
>   let freeIds = freeTypeIds ts
>   addTypeFrameArgs' (-2-numArgs) freeIds
>   return $ length freeIds
>   where addTypeFrameArgs' _ [] = return ()
>         addTypeFrameArgs' i ((Id _ id):ids) = do
>           addArg id (toInteger i)
>           addTypeFrameArgs' (i-1) ids

> freeTypeIds :: [GramType] -> [GramId]
> freeTypeIds ts = nub $ concat $ map freeTypeVars ts



Standard library

> generatePrint :: Maybe GramType -> Environment ()
> generatePrint Nothing = do
>   write "print: lds -2\nldh -1\nldr PC\nadd\nstr PC"
>   write "bra __print_char\nbra __print_int\nbra __print_bool\nbra __print_list\nbra __print_tuple\nbra __exc_untyped_variable\nbra __exc_unknown_type_frame_print"
> generatePrint (Just (GramBasicType _ IntType))  = write "__print_int: lds -1\ntrap 0\nret"
> generatePrint (Just (GramBasicType _ CharType)) = do
>   label "__print_char"
>   printChar '\''
>   write "lds -1\ntrap 1"
>   printChar '\''
>   write "ret"
> generatePrint (Just (GramBasicType _ BoolType)) = do
>   write "__print_bool: lds -1\nbrf __print_bool_false"
>   printText "True"
>   write "ret"
>   label "__print_bool_false"
>   printText "False"
>   write "ret"
> generatePrint (Just (GramListType _ _)) = do
>   label "__print_list"
>   write "lds -2\nldh 0\nldh -1\nldc 3\neq\nbrt __print_str" -- "overload" for [Char]
>   printChar '['
>   write "lds -1\nbrf __print_list_post" -- check for empty list
>   write "__print_list_elem: lds -2\nldh 0" -- load inner type frame
>   write "lds -2\nldh -1" -- load head
>   write "bsr print\najs -2" -- print head 
>   write "lds -1\nldh 0\nbrf __print_list_post" -- close singleton list
>   printText ", "
>   write "lds -1\nldh 0\nsts -2\nbra __print_list_elem" -- for longer lists, recurse
>   label "__print_list_post"
>   printChar ']'
>   write "ret"
>   label "__print_str" -- "overload" for [Char]
>   printChar '"'
>   write "lds -1\nbrf __print_str_post" -- check for empty string
>   write "__print_str_elem: lds -1\nldh -1\ntrap 1" -- print head 
>   write "lds -1\nldh 0\nbrf __print_str_post" -- check for end of list
>   write "lds -1\nldh 0\nsts -2\nbra __print_str_elem" -- if more characters remain, recurse 
>   label "__print_str_post"
>   printChar '"'
>   write "ret"
> generatePrint (Just (GramTupleType _ _ _)) = do
>   label "__print_tuple"
>   printChar '('
>   write "lds -2\nldh 0\nldh -1" -- load left type frame
>   write "lds -2\nldh -1" -- load left element
>   write "bsr print\najs -2" -- print left element
>   printText ", "
>   write "lds -2\nldh 0\nldh 0" -- load right type frame
>   write "lds -2\nldh 0" -- load right element
>   write "bsr print\najs -2" -- print right element
>   printChar ')'
>   write "ret"   

> generatePrintln :: Environment ()
> generatePrintln = do
>   write "\n\n; define println"
>   write "println: lds -2\nlds -2\nbsr print\najs -2"
>   printChar '\n'
>   write "ret"

> generateDisplay :: Environment ()
> generateDisplay = do
>   write "\n\n; define display"
>   write "display: lds -2\nldh -1\nldr PC\nadd\nstr PC"
>   write "bra __display_char\nbra __exc_display\nbra __exc_display\nbra __display_list\nbra __exc_display\nbra __exc_display\nbra __exc_unknown_type_frame_print"
>   write "__display_char: lds -1\ntrap 1\nret"
>   write "__display_list: lds -2\nldh 0\nldh -1\nldc 3\neq\nbrf __exc_display"
>   write "lds -1\nbrf __display_str_post" -- check for empty string
>   write "__display_str_elem: lds -1\nldh -1\ntrap 1" -- print head 
>   write "lds -1\nldh 0\nbrf __display_str_post" -- check for end of list
>   write "lds -1\nldh 0\nsts -2\nbra __display_str_elem" -- if more characters remain, recurse 
>   write "__display_str_post: ret"

> callEquals :: GramType -> Environment ()
> callEquals (GramBasicType _ CharType) = write "eq"
> callEquals (GramBasicType _ IntType) = write "eq"
> callEquals (GramBasicType _ BoolType) = write "bsr __eq_bool\najs -2\nldr RR"
> callEquals t@(GramListType _ _) = do
>   typeFrame t
>   write "lds -2\nlds -2\nbsr __eq_list\najs -5\nldr RR"
> callEquals t@(GramTupleType _ _ _) = do
>   typeFrame t
>   write "lds -2\nlds -2\nbsr __eq_tuple\najs -5\nldr RR"
> callEquals t@(GramIdType _) = do
>   typeFrame t
>   write "lds -2\nlds -2\nbsr __eq\najs -5\nldr RR"
> callEquals _ = write "bra __exc_unknown_type_frame_eq"

> generateEquals :: Maybe GramType -> Environment ()
> generateEquals Nothing = do
>   write "__eq: lds -3\nldh -1\nldr PC\nadd\nstr PC"
>   write "bra __eq_char\nbra __eq_int\nbra __eq_bool\nbra __eq_list\nbra __eq_tuple\nbra __exc_untyped_variable\nbra __exc_unknown_type_frame_eq"
> generateEquals (Just (GramBasicType _ CharType)) = write "__eq_char: lds -2\nlds -2\neq\nstr RR\nret"
> generateEquals (Just (GramBasicType _ IntType))  = write "__eq_int: lds -2\nlds -2\neq\nstr RR\nret"
> generateEquals (Just (GramBasicType _ BoolType)) = do
>   write "__eq_bool: lds -2\nldc 0\nne\nlds -2\nldc 0\nne" -- convert to "unequal to zero" representation as -1 and 1 are both used by SSM
>   write "eq\nstr RR\nret"
> generateEquals (Just (GramListType _ _)) = do
>   write "__eq_list: lds -2\nlds -2\nbrf __eq_list_null\nbrf __eq_list_retfalse" -- check for null lists (base case)
>   write "lds -3\nldh 0" -- load inner type frame
>   write "lds -3\nldh -1\nlds -3\nldh -1" -- load both heads
>   write "bsr __eq\najs -3\nldr RR\nbrf __eq_list_retfalse" -- compare heads
>   write "lds -3\nlds -3\nldh 0\nlds -3\nldh 0" -- copy type frame and list tails
>   write "bsr __eq_list\najs -3\nret" -- recursive call on tails
>   write "__eq_list_null: ldc 0\neq\nbra __eq_list_post" -- first list empty - return isEmpty(second list)
>   write "__eq_list_retfalse: ldc 0" -- first list non-empty, second empty - return false
>   write "__eq_list_post: str RR\nret"
> generateEquals (Just (GramTupleType _ _ _)) = do
>   write "__eq_tuple: lds -3\nldh 0\nldh -1" -- load left element's type frame
>   write "lds -3\nldh -1\nlds -3\nldh -1" -- load both left elements
>   write "bsr __eq\najs -3\nldr RR\nbrf __eq_tuple_retfalse" -- compare left elements
>   write "lds -3\nldh 0\nldh 0" -- load right element's type frame
>   write "lds -3\nldh 0\nlds -3\nldh 0" -- load both right elements
>   write "bsr __eq\najs -3\nret" -- compare right elements
>   write "__eq_tuple_retfalse: ldc 0\nstr RR\nret"

> generateIsEmpty :: Environment ()
> generateIsEmpty = do
>   write "\n\n; define isEmpty"
>   write "isEmpty: lds -1\nldc 0\neq\nstr RR\nret"

> generateChrOrd :: Environment ()
> generateChrOrd = do
>   write "\n\n; define chr"
>   write "chr: lds -1\nstr RR\nret"
>   write "\n\n; define ord"
>   write "ord: lds -1\nstr RR\nret"

> generateError :: Environment ()
> generateError = do
>   write "\n\n; define error"
>   label "error"
>   printText "The code threw a runtime error: "
>   typeFrame $ GramListType undefined $ GramBasicType undefined CharType
>   write "lds -2\nbsr print\nldc 10 ; character '\\n'\ntrap 1\nhalt"


Standard library handlers

> builtinNames = ["print", "println", "display", "isEmpty", "chr", "ord", "error"]

> builtins = let und = undefined in
>   [polyBuildIn "print" generatePrint,
>    polyBuildIn "==" generateEquals,
>    generatePrintln,
>    generateDisplay,
>    generateIsEmpty,
>    generateChrOrd,
>    generateError,
>    runTimeException "__exc_empty_list_traversal" "empty list traversed",
>    runTimeException "__exc_untyped_variable" "could not resolve overloading for print",
>    runTimeException "__exc_unknown_type_frame_print" "functions cannot be printed or displayed",
>    runTimeException "__exc_unknown_type_frame_eq" "equality is not defined for functions",
>    runTimeException "__exc_display" "display can only be used for characters and character lists",
>    runTimeException "__exc_glob_fun_assignment" "global functions cannot be overwritten",
>    runTimeException "__exc_unknown_error" "an unknown error occurred"]

> polyBuildIn :: String -> (Maybe GramType -> Environment ()) -> Environment ()
> polyBuildIn s f = do
>   let und = undefined
>   let types = [Nothing, Just $ GramBasicType und CharType, Just $ GramBasicType und IntType, Just $ GramBasicType und BoolType, 
>                Just $ GramListType und und, Just $ GramTupleType und und und]
>   write $ "\n\n; define polymorphic " ++ s
>   mapM_ f types


Exception handlers

> runTimeException :: String -> String -> Environment ()
> runTimeException lab s = do
>   write $ "\n\n; Runtime exception: " ++ s
>   label lab
>   printChar '\n'
>   printText $ "A runtime exception occurred: " ++ s
>   printChar '\n'
>   write "halt"

> printText :: String -> Environment ()
> printText = mapM_ printChar

> printChar :: Char -> Environment ()
> printChar c = write $ "ldc " ++ show (ord c) ++ " ; character " ++ show c ++ "\ntrap 1"


Post-processing

> postprocess :: Code -> Code
> postprocess =  fixGlobals . fixDoubleLabels . fixEmptyLabels

> fixDoubleLabels :: Code -> Code -- removes faulty situations such as: "fi_6: while_7: statement"
> fixDoubleLabels code = 
>   let (lns, subs) = unzip $ map (findDoubleLabels . words) $ lines code in
>   let substitute = applySub $ concat subs in
>   unlines $ map (substitute . unwords) lns
>   where findDoubleLabels wrds
>           | length wrds > 2 && last (head wrds) == ':' && last (head $ tail wrds) == ':' = 
>               let (newwrds, [(from, to)]) = (tail wrds, [(init $ head wrds, init $ head $ tail wrds)]) in
>               let (finalwrds, othersubs) = findDoubleLabels newwrds in
>               (finalwrds, (from, subbedLabel to othersubs) : othersubs)
>           | otherwise = (wrds, [])
>         subbedLabel to [] = to
>         subbedLabel _ othersubs = snd $ last othersubs
>         applySub subs ln
>           | any (\i -> isPrefixOf i ln) subbedInstructions = 
>             case lookup (drop 4 ln) subs of
>               Just lab -> (take 4 ln) ++ lab
>               Nothing  -> ln
>           | otherwise = ln
>         subbedInstructions = ["bra ", "brf ", "brt ", "bsr ", "ldc "]

> fixEmptyLabels :: Code -> Code -- removes errors coming from non-void functions ending in stray labels "fi_6:", e.g. when returning within if statement
> fixEmptyLabels code =
>   let (lns, emptyLabels) = unzip $ map getEmptyLabel $ lines code in
>   unlines $ filter (not . containsLabels (concat emptyLabels)) $ concat lns
>   where hasJustLabels ln = all (\w -> last w == ':') $ words ln
>         getEmptyLabel ln
>           | length ln >= 2 && hasJustLabels ln = ([], map init $ words ln)
>           | otherwise = ([ln], [])
>         containsLabels labs ln = elem ln $ concat $ map subbedInstructions labs
>         subbedInstructions lab = map (++lab) ["bra ", "brf ", "brt ", "bsr ", "ldc "]

> fixHeapFunctionCalls :: Code -> Code                                                            -- a function pointer is a tuple (code address, environment).
> fixHeapFunctionCalls load = let lns = lines load in init $ unlines $ fixHeapFunctionCalls' lns  -- we need this for function variables, but for global functions
>   where fixHeapFunctionCalls' lns                                                               -- tuple is created anew at every function call.
>           | length lns >= 4 && (last $ init lns) == "stmh 2" && isPrefixOf "ldh " (last lns) =  -- this takes up unnecessary heap space.
>             let ind = length lns - 3 - (digitToInt $ last $ last lns) in                        -- since it is an easy pattern to recognise,
>               (take (length lns - 4) lns) ++ [lns !! ind]                                       -- we get rid of the tuple creation and extract 
>           | otherwise = lns                                                                     -- only the line we need (either code address or environment)

> collapseConstantBranches :: Code -> Code
> collapseConstantBranches call = 
>   let lns = lines call in
>   let (prep, jump) = splitAt (length lns - 2) lns in
>   init $ unlines $ prep ++ collapseConstantBranches' jump -- the init, here and in fixHeapFunctionCalls,
>   where collapseConstantBranches' jlns                    -- removes a trailing newline added by unlines
>           | length jlns == 2 && isPrefixOf "ldc " (head jlns) && last jlns == "jsr" = ["bsr " ++ drop 4 (head jlns)]
>           | otherwise = jlns

> fixGlobals :: Code -> Code
> fixGlobals code = foldr (\(g,a) c -> replaceGlobal c g a) code (globalAddresses code)
>   where
>     firstAddress c = 17 + (addressesByProgram $ cleanCode c)
>     globals c = map (\\ tag) $ filter (isPrefixOf tag) $ lines c
>     globalAddresses c = zip (globals c) [firstAddress (lines c)..]
>     tag = "; initialise global: "

> cleanCode :: [String] -> [String]
> cleanCode = removeEmptyLines . removeLineComments . removeSimpleComments
>   where
>     removeLineComments = (filter (\x -> not (isPrefixOf ";" x)))
>     removeSimpleComments = (map (takeWhile (/= ';')))
>     removeEmptyLines = filter (\x -> (x /= ""))

> addressesByProgram :: [String] -> Int
> addressesByProgram [] = 0
> addressesByProgram (x:xs) = addressesByInst x + addressesByProgram xs

> addressesByInst :: String -> Int
> addressesByInst ('$':'_':_) = 2
> addressesByInst s = length (words (takeWhile (/= ';') s))

> replaceGlobal :: Code -> String -> Int -> Code
> replaceGlobal code global address = replace ("$_" ++ global) loadGlobal code
>   where
>     loadGlobal = "ldc " ++ show address ++ " ; load global " ++ global

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
>   let loadIns = "ldl " ++ show d ++ " ; variable " ++ id
>       storeIns = "stl " ++ show d ++ " ; variable " ++ id
>       addressIns = "ldla " ++ show d ++ " ; variable " ++ id
>   put ((d+1,(id, (loadIns, storeIns, addressIns)):v):ss, i)

> addGlobal :: Id -> Environment ()
> addGlobal id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "$_" ++ id ++ "\nlda 0"
>       storeIns = "$_" ++ id ++ "\nsta 0"
>       addressIns = "$_" ++ id
>   put ((d+1,(id, (loadIns, storeIns, addressIns)):v):ss, i)

> addGlobalFunc :: Id -> Environment ()
> addGlobalFunc id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldc " ++ id ++ "\nldc 0 ; null environment\nstmh 2"
>       storeIns = "bra __exc_glob_fun_assignment"
>       addressIns = "bra __exc_glob_fun_assignment"
>   put ((d,(id, (loadIns, storeIns, addressIns)):v):ss, i)

> addArg :: Id -> Integer -> Environment ()
> addArg varId id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldl " ++ show id ++ " ; argument " ++ varId
>       storeIns = "stl " ++ show id ++ " ; argument " ++ varId
>       addressIns = "ldla " ++ show id ++ " ; argument " ++ varId
>   put ((d,(varId, (loadIns, storeIns, addressIns)):v):ss, i)

> lookupVar :: Id -> Environment (Code, Code, Code)
> lookupVar varId = do
>   (ss, _) <- get
>   return $ lookupVar' varId ss
>   where lookupVar' _ [] = ("bra __exc_unknown_error", "bra __exc_unknown_error", "bra __exc_unknown_error")
>         lookupVar' varId ((_,v):vs) =
>           case lookup varId v of
>             Nothing -> lookupVar' varId vs
>             Just d -> d

> replaceVar :: Id -> (Code, Code, Code) -> Environment ()
> replaceVar varId codes = do
>   (ss, i) <- get
>   put (replaceVar' varId codes ss, i)
>   where replaceVar' _ _ [] = []
>         replaceVar' varId codes (sc@(d,scope):scopes) =
>           case lookup varId scope of
>             Nothing -> sc : replaceVar' varId codes scopes
>             Just _  -> (d, replaceVar'' varId codes scope) : scopes
>         replaceVar'' varId codes scope = (varId, codes) : filter (\(id,_) -> id /= varId) scope



Capture and environment handlers

> getCapture :: [Capture] -> GramId -> Capture
> getCapture capts fid = capt
>   where matchId fid (Capture cid _ _) = fid == cid
>         Just capt = find (matchId fid) capts

> captureVar :: GramId -> Environment ()
> captureVar (Id _ vid) = do
>   (load,store,_) <- lookupVar vid
>   write load
>   write $ "sth ; capture " ++ vid
>   write store
>   let newload = load ++ "\nldh 0"
>   let newstore = load ++ "\nsta 0"
>   let newaddr = load
>   replaceVar vid (newload, newstore, newaddr)

> captureIfNeeded :: Capture -> GramId -> Environment ()
> captureIfNeeded (Capture _ captures nestedcapts) id
>   | id `elem` capturedbynested && not (id `elem` captures) = captureVar id
>   | otherwise = return ()
>   where capturedbynested = nub $ concat $ map getCaptured nestedcapts
>         getCaptured = \(Capture _ captured _) -> captured

> functionEnvironment :: [Capture] -> GramId -> Environment ()
> functionEnvironment capts id = let capt = getCapture capts id in functionEnvironment' capt
>   where functionEnvironment' (Capture _ [] _) = write "ldc 0 ; null environment"
>         functionEnvironment' (Capture _ captured _) = functionEnvironment'' captured
>         functionEnvironment'' [Id _ vid] = do
>           (_,_,addr) <- lookupVar vid
>           write addr
>         functionEnvironment'' captured = do
>           let (l, r) = splitAt ((length captured + 1) `div` 2) captured
>           functionEnvironment'' l
>           functionEnvironment'' r
>           write "stmh 2"

> addEnvVar :: Id -> Integer -> Integer -> Environment ()
> addEnvVar varId envSize id = do
>   (((d,v):ss), i) <- get
>   (loadEnv,_,_) <- lookupVar "__env"
>   let loadAddrFromEnv = sep loadEnv $ getFromEnv envSize id
>       loadIns = loadAddrFromEnv ++ "\nldh 0"
>       storeIns = loadAddrFromEnv ++ "\nsta 0"
>       addressIns = loadAddrFromEnv
>   put ((d,(varId, (loadIns, storeIns, addressIns)):v):ss, i)
>   where getFromEnv 1 _ = ""
>         getFromEnv numVars i 
>           | i+1 <= lsize = sep "ldh -1" $ getFromEnv lsize i
>           | otherwise = sep "ldh 0" $ getFromEnv (numVars-lsize) (i-lsize)
>           where lsize = (numVars + 1) `div` 2
>         sep s "" = s
>         sep s1 s2 = s1 ++ "\n" ++ s2

> addFunctionEnvironment :: Capture -> Environment ()
> addFunctionEnvironment (Capture _ captured _) = addFunctionEnvironment' captured (toInteger $ length captured) 0
>   where addFunctionEnvironment' [] _ _ = return ()
>         addFunctionEnvironment' ((Id _ id):captured) envSize i = do
>           addEnvVar id envSize $ toInteger i
>           addFunctionEnvironment' captured envSize (i+1)


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
