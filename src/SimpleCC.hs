module SimpleCC (compile)
where

import Control.Monad.State hiding (join)

import Simple.Ast
import Simple.Parse

import Twodee.Ast

-- Compilation from Simple values to 2d values. Not exhaustive.
compileV Zero = Inr Unit
compileV (Succ e) = Inl (compileV e)

type Layout = Joint
type Supply a = State Int a

new :: State Int Int
new = do
  i <- get
  put (i+1)
  return i

compile :: Ast -> Supply Layout
compile Zero = return $ (mkBox (Send1 (Inr Unit) E))
compile (Succ x) =
    do
      cx <- compile x
      join_ew cx (mkBox (Send1 (Inl (Iface W)) E))
compile (Plus e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      join c1 c2 (mkBox (Use "plus"))
compile (Mul e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      join c1 c2 (mkBox (Use "mul"))

join_ew :: Layout -> Layout -> Supply Layout
join_ew b1 b2 = do
  wire <- new
  b1' <- return $ b1 { b_east = Just wire }
  b2' <- return $ b2 { west = Just wire }
  return $ mkBoxGrp [b1', b2']

join :: Layout -> Layout -> Layout -> Supply Layout
join wi ni box = do
  wire1 <- new
  wire2 <- new
  wi' <- return $ wi { b_east = Just wire1 }
  ni' <- return $ ni { b_south = Just wire2 }
  box' <- return $ box { north = Just wire2,
                         west = Just wire1 }
  return $ mkBoxGrp [wi', ni', box']

