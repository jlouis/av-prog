module Simple.Ast (Ast(..),
            eval,
            astPrint)
where

import Data.List
import Text.PrettyPrint
import Data.Map as M

-- Syntax of the small language
data Ast = Zero | Succ Ast | Plus Ast Ast | Mul Ast Ast

-- Evaluation operation
eval Zero = Zero
eval (Succ e) = Succ (eval e)
eval (Plus Zero e) = eval e
eval (Plus e Zero) = eval e
eval (Plus (Succ n) e) = Succ (eval (Plus n e))
-- If either is 0, then the result is 0
eval (Mul Zero e) = Zero
eval (Mul e Zero) = Zero
-- If one of them is 1, then the result is the other
eval (Mul (Succ Zero) e) = eval e
eval (Mul e (Succ Zero)) = eval e
-- Otherwise I let the result be the following
{- We do not need a second case, since the first value cannot be Zero or (Succ Zero) - 
otherwise it would have been caught by one of the above statements -}
eval (Mul (Succ e) n) = eval (Plus n (eval (Mul e n)))


astPrint :: String -> Ast -> String
astPrint str Zero = "z"
astPrint str (Succ e) = str ++ "s " ++ result where
    result = astPrint str e
astPrint str (Plus e1 e2) = str ++ r1 ++ " + " ++ r2 where
    r1 = astPrint "" e1;
    r2 = astPrint "" e2
astPrint str (Mul e1 e2) = str ++ r1 ++ " * " ++ r2 where
    r1 = astPrint "" e1;
    r2 = astPrint "" e2