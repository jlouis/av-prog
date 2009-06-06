module Simple.Main (main) where

import Simple.Parse
import Simple.Ast

main = putStrLn value where   --putStrLn (astPrint "" ast) where
    value = astPrint "" (eval ast)
    ast = parsePrg "((s (s z)) + ((s z) + (s z))) * z"