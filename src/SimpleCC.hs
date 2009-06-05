module SimpleCC ()
where

import Simple.Ast
import Simple.Parse

import Twodee.Ast

-- Compilation from Simple values to 2d values. Not exhaustive.
compileV Zero = Inr Unit
compileV (Succ e) = Inl (compileV e)

compile Zero = MkBox (\n w -> Send1 (Inr Unit) E)
compile (Succ x) =
    let cx = compile x
    in
      ew_wire cx (MkBox (\n w -> Send1 (Inl (Iface w)) E))
compile (Plus e1 e2) =
    let c1 = compile e1
        c2 = compile e2
    in
      MkBox (\n w -> Use "plus")

ew_wire :: Box -> Box -> Box
ew_wire b1 b2 = MkBox (\e w -> SendEmpty)

