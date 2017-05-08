> module Generator where

> import Grammar (Gram)

> generate :: Gram -> String
> generate _ = "LDC 2\nLDC 3\nADD" ++ "\nTRAP 0"