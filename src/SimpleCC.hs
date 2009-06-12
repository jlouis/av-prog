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

mkBox :: Command -> Supply Layout
mkBox c = do
  wn <- new
  we <- new
  ww <- new
  ws <- new
  return $ JBox { command = c,
                  north = wn,
                  east = we,
                  west = ww,
                  south = ws }

mkModule :: String -> Supply Layout -> Supply Mod
mkModule module_name jnt = do
  wn <- new
  ww <- new
  we <- new
  bxs <- jnt
  return $ Module { mod_boxes = boxes bxs,
                    name = module_name,
                    input_north = wn,
                    input_west  = ww,
                    output_east = we }

mkBoxGrp w n e s grp =
    return $ JBox_Group { boxes = grp,
                          b_north = n,
                          b_south = s,
                          b_east  = e,
                          b_west  = w }

compile :: Ast -> Supply Layout
compile Zero = mkBox (Send1 (Inr Unit) E)
compile (Succ x) =
    do
      cx <- compile x
      b <- mkBox (Send1 (Inl (Iface W)) E)
      join_ew cx b
compile (Plus e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      b <- (mkBox (Use "plus"))
      join c1 c2 b
compile (Mul e1 e2) =
    do
      c1 <- compile e1
      c2 <- compile e2
      b <- (mkBox (Use "mul"))
      join c1 c2 b

join_ew :: Layout -> Layout -> Supply Layout
join_ew b1 b2 = do
  wire <- return $ b_east b1
  b2' <- return $ b2 { west = wire }
  mkBoxGrp (b_west b1) (north b2) (east b2) (south b2) [b1, b2']

join :: Layout -> Layout -> Layout -> Supply Layout
join wi ni box = do
  wire1 <- return $ b_east wi
  wire2 <- return $ b_north ni
  box' <- return $ box { north = wire2,
                         west = wire1 }
  mkBoxGrp (b_west wi) (b_north ni) (east box') (south box') [wi, ni, box']

