module Simple.Ast (Ast(..), EnvValue(..), ConstValue(..), FuncEnvValue(..), FuncInstValue(..),
            start,
            astPrint,
            envPrint)
where

import Data.List

-- Syntax of the small language
data Ast = Zero | Succ Ast
         | Plus Ast Ast | Mul Ast Ast
         | Lookup String | Call String Ast
         | Contz Ast Ast Ast
         | Pre Ast

-- The environment
data EnvValue = Env ConstValue EnvValue | EnvEnd
data ConstValue = Const String Ast

-- The functions
data FuncEnvValue = FuncEnv FuncInstValue FuncEnvValue | FuncEnd
data FuncInstValue = Func String Ast EnvValue



-- Evaluation operation
eval Zero _ _ = Zero
eval (Succ e) env fkt = Succ (eval e env fkt)
eval (Plus Zero e) env fkt = eval e env fkt
eval (Plus e Zero) env fkt = eval e env fkt
eval (Plus (Succ n) e) env fkt = Succ (eval (Plus n e) env fkt)

eval (Plus e1 e2) env fkt = eval (Plus (eval e1 env fkt) (eval e2 env fkt)) env fkt

-- If either is 0, then the result is 0
eval (Mul Zero _) _ _ = Zero
eval (Mul _ Zero) _ _ = Zero

-- If one of them is 1, then the result is the other
eval (Mul (Succ Zero) e) env fkt = eval e env fkt
eval (Mul e (Succ Zero)) env fkt = eval e env fkt

-- Otherwise I let the result be the following
{- We do not need a second case, since the first value cannot be Zero or (Succ Zero) -
   otherwise it would have been caught by one of the above statements -}
eval (Mul (Succ e) n) env fkt = eval (Plus n (eval (Mul e n) env fkt)) env fkt

eval (Mul e1 e2) env fkt = eval (Mul (eval e1 env fkt) (eval e2 env fkt)) env fkt

-- Lookup mechanism
eval (Lookup str) env fkt = envLookup str env env fkt
eval (Call str input) env fkt = fktCall str env (eval input env fkt) fkt fkt
eval (Contz Zero first _) env fkt = eval first env fkt
eval (Contz (Succ k) _ second) env fkt = eval second (Env (Const "k"  k) env) fkt
eval (Contz check first second) env fkt = eval (Contz (eval check env fkt) first second) env fkt
---eval (Pre Zero) _ _ = Zero
--eval (Pre (Succ c)) env fkt = eval c env fkt
--eval (Pre e) env fkt = eval (Pre (eval e env fkt)) env fkt

envLookup _ EnvEnd _ _ = Zero
envLookup str (Env (Const id calc) env) constantEnv fkt =
                                               if str == id
                                               then eval calc constantEnv fkt
                                               else envLookup str env constantEnv fkt
fktCall _ _ _ FuncEnd _ = Zero
fktCall str env input (FuncEnv (Func id ast _localEnv) funcValue) constantFkt =
                                               if str == id
                                               then eval ast (Env (Const "input" input) env) constantFkt
                                               else fktCall str env input funcValue constantFkt

-- Program mechanism
start ast env = eval ast env

astPrint :: String -> Ast -> String
astPrint _ Zero = "z"
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
envPrint _ EnvEnd = "End; "
envPrint str (Env const env) = str ++ r1 ++ r2 where
    r1 = constPrint "" const;
    r2 = envPrint "" env

constPrint :: String -> ConstValue -> String
constPrint str (Const id value) = str ++ id ++ " " ++ r ++ " " where
    r = astPrint "" value