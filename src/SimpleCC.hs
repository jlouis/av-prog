module SimpleCC (compile)
where

import Control.Monad.State hiding (join)

import Simple.Ast
import Simple.Parse

import Twodee.Ast

-- Compilation from Simple values to 2d values. Not exhaustive.
compileV Zero = Inr Unit
compileV (Succ e) = Inl (compileV e)

type Layout = [Box]
type Supply a = State Int a

singleton x = [x]

compile :: Ast -> Supply Layout
compile Zero = return $ singleton $ (mkBox (Send1 (Inr Unit) E))
compile (Succ x) =
    do
      cx <- compile x
      join_ew cx (singleton (mkBox (Send1 (Inl (Iface W)) E)))
compile (Plus e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      join c1 c2 (singleton (mkBox (Use "plus")))
compile (Mul e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      join c1 c2 (singleton (mkBox (Use "mul")))

join_ew :: Layout -> Layout -> Supply Layout
join_ew b1 b2 = return []

join :: Layout -> Layout -> Layout -> Supply Layout
join wi ni box = return []


