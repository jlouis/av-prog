module Simple.Ast (Ast(..),
            eval)
where

-- Syntax of the small language
data Ast = Zero | Succ Ast | Plus Ast Ast

-- Evaluation operation
eval Zero = Zero
eval (Succ e) = Succ (eval e)
eval (Plus Zero e) = eval e
eval (Plus (Succ n) e) = Succ (eval (Plus n e))
