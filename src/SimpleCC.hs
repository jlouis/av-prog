module SimpleCC (compile)
where

import Simple.Ast
import Simple.Parse

import Twodee.Ast

-- Compilation from Simple values to 2d values. Not exhaustive.
compileV Zero = Inr Unit
compileV (Succ e) = Inl (compileV e)

compile :: Ast -> [Joint]
compile Zero = [JBox $ MkBox (\n w -> Send1 (Inr Unit) E)]
compile (Succ x) =
    let cx = compile x
    in
      ew_join cx (MkBox (\n w -> Send1 (Inl (Iface w)) E))
compile (Plus e1 e2) =
    let c1 = compile e1
        c2 = compile e2
    in [JBox $ MkBox (\n w -> Use "plus")]
compile (Mul e1 e2) =
    let c1 = compile e1
        c2 = compile e2
    in [JBox $ MkBox (\n w -> Use "mul")]


ew_join :: [Joint] -> Box Inface -> [Joint]
ew_join [] b = [JSpacing, JBox b]
ew_join (box : boxes) b = box : ew_join boxes b


