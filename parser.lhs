> import Prelude hiding ((>>))
> import Lexer

> type Parser t s = [t] -> [(s, [t])]

> data GramFunType = 
>     GramFunType GramFTypes GramRetType
>   deriving (Show)

> data GramFTypes =
>     GramFTypes GramType GramFTypes
>   | GramEmpty
>   deriving (Show)

> data GramRetType =
>     GramRetType GramType
>   | GramVoidType
>   deriving (Show)

> data GramType =
>     GramBasicType BasicType
>   | GramTupleType GramType GramType
>   | GramListType GramType
>   | GramCustomType String
>   deriving (Show)


The combinator-less versions are left so it's maybe easier to understand.
Will remove them before handing it in though.

> parseType :: Parser Token GramType
> parseType = parseBasicType ||| parseListType ||| parseTupleType ||| parseCustomType

> parseBasicType :: Parser Token GramType
> parseBasicType = isBasicType |-> idP ->- (\(TokenType bt, lst) -> (GramBasicType bt, lst))
>   where 
>     isBasicType :: Token -> Bool
>     isBasicType (TokenType _) = True
>     isBasicType _ = False

parseBasicType ((TokenType bt):toks) = [(GramBasicType bt, toks)]
parseBasicType _ = []

> parseTupleType :: Parser Token GramType
> parseTupleType = (parseL -+- parseR) ->- (\(typtup, rem) -> (GramTupleType (fst typtup) (snd typtup), rem))
>   where
>     parseL = (((==) TokenComma) ->>| ((==) TokenOpenP) |->> parseType)
>     parseR = ((==) TokenCloseP) ->>| parseType

parseTupleType :: Parser Token GramType
parseTupleType (TokenOpenP:toks) = parseRightTupleType [(innertype, rem) | (innertype, nxt:rem) <- parseType toks, nxt == TokenComma]
parseTupleType _ = []

parseRightTupleType :: [(GramType, [Token])] -> [(GramType, [Token])]
parseRightTupleType lst = [(GramTupleType lefttype righttype, remright) | (lefttype, remleft) <- lst, (righttype, nxt:remright) <- parseType remleft, nxt == TokenCloseP]

 
> parseListType :: Parser Token GramType
> parseListType = ((==) TokenCloseSquareB) ->>| ((==) TokenOpenSquareB) |->> parseType ->- (\(typ, rem) -> (GramListType typ, rem))

parseListType (TokenOpenSquareB:toks) = [(GramListType innertype, rem) | (innertype, nxt:rem) <- parseType toks, nxt == TokenCloseSquareB]
parseListType _ = []

> parseCustomType :: Parser Token GramType
> parseCustomType = (isCustomType |-> idP) ->- (\(TokenId str, lst) -> (GramCustomType str, lst))
>   where 
>     isCustomType :: Token -> Bool
>     isCustomType (TokenId _) = True
>     isCustomType _ = False

parseCustomType ((TokenId str):toks) = [(GramCustomType str, toks)]
parseCustomType _ = []



The part below might have to be moved to another file, we'll discuss.
Feel free to suggest notational changes as well

=== Parser combinators ===

Utility functions:
idP is the identity parse: takes the top token and yields it as result directly.
tl is the protected tail function, which doesn't crash on empty lists.

> idP :: [t] -> [(t, [t])]
> idP [] = []
> idP (x:xs) = [(x, xs)]

> tl :: [t] -> [t]
> tl [] = []
> tl l = tail l


Choice combinator: A = B | C
Usage: parseA = parseB ||| parseC
Yields: yields results of parseB and parseC, concatenated

> (|||) :: (Parser t s) -> (Parser t s) -> (Parser t s)
> f ||| g = \toks -> (f toks) ++ (g toks)
> infixl 2 |||


Consume token combinator: A = t B
Usage: parseA = (>>) parseB
Yields: the result of parseB after token t has been discarded

> (>>) :: (Parser t s) -> (Parser t s)
> (>>) parser = \toks -> case toks of
>                                [] -> []
>                                (x:xs) -> parser xs

Initial/terminal condition combinator: A = c B / A = B c
Usage: parseA = isC |-> parseB or parseA = isC ->| parseB
Yields: result parseB only if preceded / followed by a token that satisfies isC.
The double-arrow versions consume/discard token c in the process, whereas the single-arrow
versions do not (enabling look-ahead).

> (|->) :: (t -> Bool) -> (Parser t s) -> (Parser t s)
> pred |-> parser = \toks -> case toks of
>                                 []     -> []
>                                 (x:xs) | pred (head toks) -> parser toks
>                                        | otherwise        -> []
> infixr 5 |->

> (->|) :: (t -> Bool) -> (Parser t s) -> (Parser t s)
> endpred ->| parser = \toks -> [(x, ys) | (x, ys) <- parser toks, endpred (head ys)]
> infixr 5 ->|

> (|->>) :: (t -> Bool) -> (Parser t s) -> (Parser t s)
> endpred |->> parser = endpred |-> (>>) parser
> infixr 5 |->>

> (->>|) :: (t -> Bool) -> (Parser t s) -> (Parser t s)
> endpred ->>| parser = endpred ->| parser ->- (\(x,y) -> (x, tl y))
> infixr 5 ->>|


Result mapping combinator
Usage: parseA ->- mapresult
Yields: tuples mapresult(x) for every result x = (GramX, [RemainingTokens]) coming out of parseA

> (->-) :: (Parser t a) -> ((a, [t]) -> (b, [t])) -> (Parser t b)
> parser ->- mapper = \toks -> [mapper tup | tup <- parser toks]
> infixl 4 ->-


Concatenation combinator: A = B C
Usage: parseA = parseB -+- parseC
Yields: tuples ((GramB,GramC), [RemainingTokens]) for every application of parseC on the remainder after parseB.
Note they still need to be combined into a higher-level grammatical structure (such as GramTupleType) with e.g. ->-

> (-+-) :: (Parser t a) -> (Parser t b) -> (Parser t (a,b))
> prsL -+- prsR = \toks -> [((xl,xr), remr) | (xl, reml) <- prsL toks, (xr, remr) <- prsR reml]
> infixl 3 -+-

Optional concatenation combinator: A = B [C]
Usage: parseA = parseB -+?- parseC
Yields: The same as for -+-, but the result is of type Either GramB (GramB,GramC).
Note a mapper is needed afterwards to map Left GramB, or Right (GramB, GramC) into their desired form.

(-+?-) :: (Parser t a) -> (Parser t b) -> (Parser t (Either a (a,b)))
prsL -+?- prsR = (prsL ->- (\(x,y) -> (Left x, y))) ||| ((prsL -+- prsR) ->- (\(x,y) -> (Right x, y)))
infixl 3 -+?-
