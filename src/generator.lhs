> module Generator where

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Writer (WriterT, tell, execWriterT)
> import Data.Char (ord)
> import Data.List (find, genericLength, intersect, isPrefixOf, nub)
> import Dependency (Capture(..))
> import Grammar
> import Text.Parsec.Pos (sourceLine, sourceColumn)
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

> generate :: [Capture] -> Gram -> Code
> generate capts prog = postprocess $ run (generateProgram capts) prog

> generateProgram :: [Capture] -> Gram -> Environment ()
> generateProgram capts prog = do 
>   write "bra __init"
>   let (globals, funcs) = sepDecls prog
>   sequence_ $ map addGlobalFunc builtinNames
>   sequence_ $ map (addGlobal . getVarId . GramDeclVar) globals
>   sequence_ $ map (addGlobalFunc . getVarId . GramDeclFun) funcs
>   sequence_ $ replicate (length globals) $ write "nop"
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

> getVarId :: GramDecl -> String
> getVarId (GramDeclVar (GramVarDeclType _ (Id _ varId) _)) = varId
> getVarId (GramDeclVar (GramVarDeclVar (Id _ varId) _)) = varId
> getVarId (GramDeclFun (GramFuncDecl (Id _ funId) _ _ _)) = funId

> generateVariable :: GramVarDecl -> Environment ()
> generateVariable (GramVarDeclVar (Id _ varId) expr) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr
>   (_, store) <- lookupVar varId
>   write store
> generateVariable (GramVarDeclType _ (Id _ varId) expr) = do
>   write $ "\n; initialise global: " ++ varId
>   generateExpr expr
>   (_, store) <- lookupVar varId
>   write store

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
>   write $ "\n; define function " ++ funId
>   pushScope
>   label funId
>   let argCounter = length args
>   if funId /= "main" then write "link 0" else return ()
>   isPoly <- addTypeFrameArgs argCounter $ getArgTypes types
>   addArg "__env" $ decrIfTrue isPoly argCounter
>   addArgs args
>   mapM_ (captureIfNeeded capt) args
>   generateStmtBlock capt stmts
>   case getFuncReturnType types of
>     (GramVoidType _) -> if funId == "main" then write "halt" else write "unlink\nret"
>     otherwise -> do return ()
>   where getArgTypes [GramFunTypeAnnot ftypes _] = ftypes
>         decrIfTrue True x = toInteger $ x-1
>         decrIfTrue _    x = toInteger x

> generateFunCall :: GramFunCall -> Environment ()
> generateFunCall (GramOverloadedFunCall ts (Id _ funId) args) = do
>   (load, _) <- lookupVar funId
>   write load
>   write "ldh 0" -- load environment
>   functionTypeFrame ts
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   write load -- actual function label
>   write $ "ldh -1\njsr\najs " ++ show (-2-length args) 
> generateFunCall (GramFunCall (Id _ funId) args) = do
>   (load, _) <- lookupVar funId
>   write load
>   write "ldh 0" -- load environment
>   let rev_args = reverse args
>   sequence $ map generateExpr rev_args
>   write load
>   write $ "ldh -1\njsr\najs " ++ show (-1-length args) 

> generateStmtBlock :: Capture -> [GramStmt] -> Environment ()
> generateStmtBlock capt stmts = do
>   pushScope
>   generateStmtBlock' capt stmts
>   where generateStmtBlock' capt [] = popScope
>         generateStmtBlock' capt (stmt:stmts) = do
>           generateStmt capt stmt
>           generateStmtBlock' capt stmts

> generateStmt :: Capture -> GramStmt -> Environment [Environment ()]
> generateStmt capt (GramWhile _ expr stmts) = do
>   wStart <- genLabel "while_start"
>   wEnd <- genLabel "while_end"
>   label wStart
>   generateExpr expr
>   write $ "brf " ++ wEnd
>   generateStmtBlock capt stmts
>   write $ "bra " ++ wStart
>   label wEnd
>   return []
> generateStmt capt (GramIf _ expr thenStmts elseStmts) = do
>   labelElse <- genLabel "else"
>   labelIf <- genLabel "if"
>   labelFi <- genLabel "fi"
>   generateExpr expr
>   write $ "brf " ++ labelElse
>   generateStmtBlock capt thenStmts 
>   write $ "bra " ++ labelFi
>   label labelElse 
>   generateStmtBlock capt elseStmts
>   label labelFi
>   return []
> generateStmt capt (GramReturn _ (Nothing)) = do
>   write "unlink\nret"
>   return []
> generateStmt capt (GramReturn _ (Just expr)) = do
>   generateExpr expr
>   write "str RR\nunlink\nret"
>   return []
> generateStmt capt (GramFunVarDecl (GramVarDeclType t id@(Id _ varId) expr)) = do
>   addVar varId
>   captureIfNeeded capt id
>   generateExpr expr
>   return []
> generateStmt capt (GramFunVarDecl (GramVarDeclVar id@(Id _ varId) expr)) = do
>   addVar varId
>   captureIfNeeded capt id
>   generateExpr expr
>   return []
> generateStmt capt (GramAttr _ (Var (Id _ varId) fields) expr) = do
>   (load, store) <- lookupVar varId
>   generateExpr expr
>   case fields of
>     [] -> write store
>     (f:_) -> do
>       write load
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
>   return [generateFunDecl nestedcapts lambdaDecl]
>   where lambdaDecl = GramFuncDecl (Id p lambdaName) args annot stmts
>         lambdaName = "_lambda_l" ++ (show $ sourceLine p) ++ "c" ++ (show $ sourceColumn p) ++ "_" ++ funId


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
> descendFields save f = case f of
>   First p fields  -> do
>     descend save "-1" fields
>   Second p fields -> do
>     descend save "0" fields
>   Head p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend save "-1" fields
>   Tail p fields   -> do
>     write "lds 0\nbrf __exc_empty_list_traversal"
>     descend save "0" fields
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
> typeFrame (GramBasicType _ CharType) = write "ldc 3\nldc 0\nstmh 2"
> typeFrame (GramBasicType _ IntType)  = write "ldc 5\nldc 0\nstmh 2"
> typeFrame (GramBasicType _ BoolType) = write "ldc 7\nldc 0\nstmh 2"
> typeFrame (GramListType _ t) = do
>   write "ldc 9"
>   typeFrame t
>   write "stmh 2"
> typeFrame (GramTupleType _ t1 t2) = do
>   write "ldc 11"
>   typeFrame t1
>   typeFrame t2
>   write "stmh 2\nstmh 2"
> typeFrame (GramIdType (Id _ id))
>   | isPrefixOf "_t" id || isPrefixOf "_v" id = do
>     (load, store) <- lookupVar $ "__tf_" ++ drop 2 id
>     write $ repl load
>   | otherwise = write "bra __exc_unknown_error"
>   where repl "bra __exc_unknown_error" = "ldc 13\nldc 0\nstmh 2"
>         repl c = c

> functionTypeFrame :: [GramType] -> Environment ()
> functionTypeFrame [t] = typeFrame t
> functionTypeFrame ts = do
>   let (l, r) = splitAt ((length ts + 1) `div` 2) ts
>   functionTypeFrame l
>   functionTypeFrame r
>   write "stmh 2"

> addTypeFrameArgs :: Int -> [GramType] -> Environment Bool
> addTypeFrameArgs locali ts = do
>   let freeIds = nub $ concat $ map freeTypeVars ts
>   addTypeFrameArgs' locali (length freeIds) 0 freeIds
>   return $ not $ null freeIds
>   where addTypeFrameArgs' :: Int -> Int -> Int -> [String] -> Environment ()
>         addTypeFrameArgs' _ _ _ [] = return ()
>         addTypeFrameArgs' locali numFrames i (id:ids) = do
>           addTypeFrameArg locali numFrames i id
>           addTypeFrameArgs' locali numFrames (i+1) ids
>         addTypeFrameArg :: Int -> Int -> Int -> String -> Environment ()
>         addTypeFrameArg locali numFrames i id = do
>           (((d,v):ss), nxt) <- get
>           let fid = "__tf_" ++ id
>           let loadIns = removeFinalNewline $ "ldl -" ++ show (2+locali) ++ "\n" ++ getTypeFrame numFrames i
>           let storeIns = "bra __exc_unknown_error"
>           put ((d,(fid, (loadIns, storeIns)):v):ss, nxt)
>         freeTypeVars (GramIdType (Id _ id)) 
>           | isPrefixOf "_v" id = [drop 2 id]
>           | otherwise = []
>         freeTypeVars (GramListType _ t) = freeTypeVars t
>         freeTypeVars (GramTupleType _ t1 t2) = freeTypeVars t1 ++ freeTypeVars t2
>         freeTypeVars _ = []
>         getTypeFrame :: Int -> Int -> String
>         getTypeFrame 1 _ = ""
>         getTypeFrame numFrames i 
>           | i+1 <= lsize = "ldh -1\n" ++ getTypeFrame lsize i
>           | otherwise = "ldh 0\n" ++ getTypeFrame (numFrames-lsize) (i-lsize)
>           where lsize = (numFrames + 1) `div` 2
>         removeFinalNewline "" = ""
>         removeFinalNewline s = init s



Standard library

> generatePrint :: Maybe GramType -> Environment ()
> generatePrint Nothing = do
>   write "print: lds -2\nldh -1\nldr PC\nadd\nstr PC"
>   write "bra __print_char\nbra __print_int\nbra __print_bool\nbra __print_list\nbra __print_tuple\nbra __exc_untyped_variable"
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

> generateEquals :: Maybe GramType -> Environment ()
> generateEquals Nothing = do
>   write "__eq: lds -3\nldh -1\nldr PC\nadd\nstr PC"
>   write "bra __eq_char\nbra __eq_int\nbra __eq_bool\nbra __eq_list\nbra __eq_tuple\nbra __exc_untyped_variable"
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
>   write "\n;define isEmpty"
>   write "isEmpty: lds -1\nldc 0\neq\nstr RR\nret\n"

> generateChrOrd :: Environment ()
> generateChrOrd = do
>   write "\n;define chr"
>   write "chr: lds -1\nstr RR\nret\n"
>   write "\n;define ord"
>   write "ord: lds -1\nstr RR\nret\n"

> generateError :: Environment ()
> generateError = do
>   write "\n; define error"
>   label "error"
>   typeFrame $ GramListType undefined $ GramBasicType undefined CharType
>   write "lds -2\nldc 10\ntrap 1\nbsr print\nhalt"


Standard library handlers

> builtinNames = ["print", "isEmpty", "chr", "ord", "error"]

> builtins = let und = undefined in
>   [polyBuildIn "print" generatePrint,
>    polyBuildIn "==" generateEquals,
>    generateIsEmpty,
>    generateChrOrd,
>    generateError,
>    runTimeException "__exc_empty_list_traversal" "Runtime exception: empty list traversed",
>    runTimeException "__exc_untyped_variable" "Runtime exception: could not resolve overloading for print",
>    runTimeException "__exc_unknown_error" "Runtime exception: an unknown error occurred"]

> polyBuildIn :: String -> (Maybe GramType -> Environment ()) -> Environment ()
> polyBuildIn s f = do
>   let und = undefined
>   let types = [Nothing, Just $ GramBasicType und CharType, Just $ GramBasicType und IntType, Just $ GramBasicType und BoolType, 
>                Just $ GramListType und und, Just $ GramTupleType und und und]
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
> printChar c = write $ "ldc " ++ show (ord c) ++ "; " ++ [c] ++ "\ntrap 1"


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

> addGlobalFunc :: Id -> Environment ()
> addGlobalFunc id = do
>   (((d,v):ss), i) <- get
>   let loadIns = "ldc " ++ id ++ "\nldc 0\nstmh 2"
>       storeIns = "bra __exc_unknown_error"
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
>   where lookupVar' _ [] = ("bra __exc_unknown_error", "bra __exc_unknown_error")
>         lookupVar' varId ((_,v):vs) =
>           case lookup varId v of
>             Nothing -> lookupVar' varId vs
>             Just d -> d

> replaceVar :: Id -> (Code, Code) -> Environment ()
> replaceVar varId codes = do
>   (ss, i) <- get
>   put (replaceVar' varId codes ss, i)
>   where replaceVar' _ _ [] = []
>         replaceVar' varId codes (sc@(d,scope):scopes) =
>           case lookup varId scope of
>             Nothing -> sc : replaceVar' varId codes scopes
>             Just _  -> (d, replaceVar'' varId codes scope) : scopes
>         replaceVar'' varId codes scope = (varId, codes) : filter (\(id,_) -> id /= varId) scope



Capture handlers

data Capture = Capture GramId [GramId] [Capture]

> getCapture :: [Capture] -> GramId -> Capture
> getCapture capts fid = capt
>   where matchId fid (Capture cid _ _) = fid == cid
>         Just capt = find (matchId fid) capts

> captureVar :: GramId -> Environment ()
> captureVar (Id _ vid) = do
>   (load,store) <- lookupVar vid
>   write load
>   write $ "sth ; captured: " ++ vid
>   write store
>   let newload = load ++ "\nldh 0"
>   let newstore = load ++ "\nlds -1\nsta 0\najs -1"
>   replaceVar vid (newload, newstore)

> captureIfNeeded :: Capture -> GramId -> Environment ()
> captureIfNeeded (Capture _ _ nestedcapts) id
>   | id `elem` capturedbynested = captureVar id
>   | otherwise = return ()
>   where capturedbynested = nub $ concat $ map getCaptured nestedcapts
>         getCaptured = \(Capture _ captured _) -> captured

> functionEnvironment :: [Capture] -> GramId -> Environment ()
> functionEnvironment capts id = let capt = getCapture capts id in functionEnvironment' capt
>   where functionEnvironment' (Capture _ [] _) = write "ldc 0"
>         functionEnvironment' (Capture _ captured _) = functionEnvironment'' captured
>         functionEnvironment'' [Id _ vid] = do
>           (load,_) <- lookupVar vid
>           write load
>         functionEnvironment'' captured = do
>           let (l, r) = splitAt ((length captured + 1) `div` 2) captured
>           functionEnvironment'' l
>           functionEnvironment'' r
>           write "stmh 2"



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
