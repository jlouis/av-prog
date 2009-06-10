module Simple.Ast (Ast(..), EnvValue(..), ConstValue(..), FuncEnvValue(..),
            start,
            astPrint,
            envPrint)
where

import Data.List
import Text.PrettyPrint
import Data.Map as M
import Control.Monad.Reader.Class

-- Syntax of the small language
data Ast = Zero | Succ Ast | Plus Ast Ast | Mul Ast Ast | Lookup String | Call String FuncEnvValue

-- The environment
data EnvValue = Env ConstValue EnvValue | EnvEnd
data ConstValue = Const String Ast

-- The functions
data FuncEnvValue = FuncEnv FuncInstValue FuncEnvValue | FuncEnd
data FuncInstValue = Func String Ast EnvValue EnvValue    

-- Evaluation operation
eval Zero = pure Zero
eval (Succ e) = pure Succ <*> (eval e)
eval (Plus Zero e) = eval e 
eval (Plus e Zero) = eval e
eval (Plus (Succ n) e) = pure Succ <*> (eval (Plus n e))

eval (Plus e1 e2) = pure eval <*> (pure Plus <*> (eval e1) <*> (eval e2))
-- If either is 0, then the result is 0
eval (Mul Zero e) = pure  Zero
eval (Mul e Zero) = pure Zero
-- If one of them is 1, then the result is the other
eval (Mul (Succ Zero) e) = eval e 
eval (Mul e (Succ Zero)) = eval e 
-- Otherwise I let the result be the following
{- We do not need a second case, since the first value cannot be Zero or (Succ Zero) -
   otherwise it would have been caught by one of the above statements -}
eval (Mul (Succ e) n) = pure eval <*> (pure Plus <*> pure n <*> (pure eval <*> (pure Mul <*> e <*> pure n)))

eval (Mul e1 e2) = pure eval <*> (pure Mul <*> (pure eval <*> e1) (pure eval <*> e2))

-- Lookup mechanism
eval (Lookup str) env fkt = envLookup str env env fkt 
--eval (Lookup str) (Env (Const id calc) env) = if str == id 
--                                               then eval calc env
--                                               else eval (Lookup str) env
--eval (Call str vars) env  = 
eval (Call str fktEnv) env fkt = fktCall str env fkt fkt

envLookup _ EnvEnd _ _ = Zero
envLookup str (Env (Const id calc) env) constantEnv fkt = 
                                               if str == id 
                                               then eval calc constantEnv fkt
                                               else envLookup str env constantEnv fkt
fktCall _ _ FuncEnd _ = Zero
fktCall str env (FuncEnv (Func id ast localEnv inputEnv) funcValue) constantFkt =
                                               if str == id
                                               then eval ast env constantFkt 
                                               else fktCall str env funcValue constantFkt

-- Program mechanism
start ast env = eval ast env

class (Functor f) => Applicative f where
   pure  :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

--instance Applicative ((->) EnvValue FuncEnvValue) where
instance Applicative ((->) env) where
    pure x = \env  -> x
    ef <*> ex = \env -> (ef env)(ex env)


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
envPrint str EnvEnd = "End; "
envPrint str (Env const env) = str ++ r1 ++ r2 where
    r1 = constPrint "" const;
    r2 = envPrint "" env

constPrint :: String -> ConstValue -> String
constPrint str (Const id value) = str ++ id ++ " " ++ r ++ " " where
    r = astPrint "" value