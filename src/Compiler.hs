-- The compiler from OCult to Twodee
module Compiler ()
where

import Twodee.Ast
import Ocult.Ast

compileInt :: Int -> Exp
compileInt 0 = Inl { e_exp = Unit }
compileInt n | n < 0 = error "Only accepting positive integers"
             | otherwise = Inr { e_exp = k_prime }
                 where
                   k_prime = compileInt (n-1)

compileTerm :: Term Int -> Exp
compileTerm (TConst k) = Inr { e_exp = n }
    where
      n = compileInt k
compileTerm (TApp t1 t2) = Inl { e_exp = Tuple { e_exp0 = t1', e_exp1 = t2' }}
    where
      t1' = compileTerm t1
      t2' = compileTerm t2

-- Pattern compiles should make a pattern into a module.



