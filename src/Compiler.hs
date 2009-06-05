-- The compiler from OCult to Twodee
module Compiler ()
where

import Twodee.Ast
import Ocult.Ast

compileInt :: Int -> Exp
compileInt 0 = Inl Unit
compileInt n | n < 0 = error "Only accepting positive integers"
             | otherwise = Inr k_prime
                 where
                   k_prime = compileInt (n-1)

compileTerm :: Term Int -> Exp
compileTerm (TConst k) = Inr n
    where
      n = compileInt k
compileTerm (TApp t1 t2) = Inl (Tuple t1' t2')
    where
      t1' = compileTerm t1
      t2' = compileTerm t2

compilePattern :: Pattern Int Int -> Mod
compilePattern _ = MkModule []

compileRule :: Rule a b -> Mod -- Perhaps wrong
compileRule _ = MkModule []

-- Pattern compiles should make a pattern into a module.
