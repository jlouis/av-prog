module Simple.Ast (Ast(..), EnvValue(..), ConstValue(..),
            start,
            astPrint,
            envPrint)
where

import Data.List
import Text.PrettyPrint
import Data.Map as M
import Control.Monad.Reader.Class

-- Syntax of the small language
data Ast = Zero | Succ Ast | Plus Ast Ast | Mul Ast Ast | Lookup String | Program EnvValue Ast

data EnvValue = Env ConstValue EnvValue | End
data ConstValue = Const String Ast

-- Evaluation operation
eval Zero _ = Zero
eval (Succ e) env = Succ (eval e env)
eval (Plus Zero e) env = eval e env
eval (Plus e Zero) env = eval e env
eval (Plus (Succ n) e) env = Succ (eval (Plus n e) env)

eval (Plus e1 e2) env = eval (Plus (eval e1 env) (eval e2 env)) env
-- If either is 0, then the result is 0
eval (Mul Zero e) _ = Zero
eval (Mul e Zero) _ = Zero
-- If one of them is 1, then the result is the other
eval (Mul (Succ Zero) e) env = eval e env
eval (Mul e (Succ Zero)) env = eval e env
-- Otherwise I let the result be the following
{- We do not need a second case, since the first value cannot be Zero or (Succ Zero) -
   otherwise it would have been caught by one of the above statements -}
eval (Mul (Succ e) n) env = eval (Plus n (eval (Mul e n) env)) env

eval (Mul e1 e2) env = eval (Mul (eval e1 env) (eval e2 env)) env
-- Lookup mechanism
eval (Lookup str) End = Zero
eval (Lookup str) (Env (Const id calc) env) = if str == id 
                                               then eval calc env
                                               else eval (Lookup str) env

-- Program mechanism
start ast env = eval ast env


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
astPrint str (Lookup id) = str ++ "Lookup: " ++ id

envPrint :: String -> EnvValue -> String
envPrint str End = "End; "
envPrint str (Env const env) = str ++ r1 ++ r2 where
    r1 = constPrint "" const;
    r2 = envPrint "" env

constPrint :: String -> ConstValue -> String
constPrint str (Const id value) = str ++ id ++ " " ++ r ++ " " where
    r = astPrint "" value