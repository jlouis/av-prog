module SimpleCC (compileF)
where

import Data.Maybe (fromJust)
import Control.Monad.State hiding (join)

import Simple.Ast

import Twodee.Ast

-- Compilation from Simple values to 2d values. Not exhaustive.
--compileV Zero = Inr Unit
--compileV (Succ e) = Inl (compileV e)

----------------------------------------------------------------------
-- Name supplies for modules
type VEnvironment = [(String, Ast)]
type Supply a = State (VEnvironment, Int, [Int]) a

new :: Supply Int
new = do
  (env, count, outlist) <- get
  put (env, count+1, outlist)
  return count

env_to_list :: EnvValue -> VEnvironment
env_to_list EnvEnd = []
env_to_list (Env (Const str ast) rest) =
    (str, ast) : (env_to_list rest)

envL str = do
  (env, _, _) <- get
  return $ fromJust $ lookup str env

compileE :: Ast -> Supply Box
compileE Zero = mkBox (Send1 (Inr Unit) E)
compileE (Succ x) =
    do
      cx <- compileE x
      b <- mkBox (Send1 (Inl (Iface W)) E)
      join_ew cx b
compileE (Plus e1 e2) =
    do
      c1 <- compileE e1
      c2 <- compileE e2
      b <- (mkBox (Use "plus"))
      join c1 c2 b
compileE (Mul e1 e2) =
    do
      c1 <- compileE e1
      c2 <- compileE e2
      b <- (mkBox (Use "mul"))
      join c1 c2 b
compileE (Lookup s) =
    do
      term <- envL s
      cx <- compileE term
      b <- mkBox (Send1 (Iface W) E)
      join_ew cx b
compileE (Contz exp cz cs) =
    do
      c1 <- compileE exp
      ccz <- compileE cz
      ccs <- compileE cs
      b <- mkBox (Case (Iface W) S E)
      grp <- join_ew c1 b
      wire_e <- return $ b_east grp
      wire_s <- return $ b_south grp
      ccz' <- return $ ccz { b_west = wire_s }
      ccs' <- return $ ccs { b_west = wire_e }
      mkBoxGrp (b_west c1) (b_north c1) (b_east ccz') (b_south ccs')
               [c1, ccz, ccs, b]
compileE (Call str ast) =
    do
      cx <- compileE ast
      b <- mkBox (Use str)
      join_ew cx b

compileF :: EnvValue -> Ast -> Mod
compileF env ast = mkModule "main" ret env'
  where
    env' = env_to_list env
    ret = do
      x <- compileE ast
      output_e x

----------------------------------------------------------------------
-- Helpers for box manipulation
----------------------------------------------------------------------

{-
mod_in_n :: Int
mod_in_n = 0

mod_in_w :: Int
mod_in_w = 1
-}

output_e x =
    do
      case x of
        JBox _ _ _ _ _ -> output $east x
        JBox_Group _ _ _ _ _ -> output $ b_east x
      return x

output w = do
  (env, count, outs) <- get
  put (env, count, w : outs)

mkModule :: String -> Supply Box -> VEnvironment -> Mod
mkModule n jnt env =
    Module { mod_boxes = [j],
             name = n,
             input_north = 0,
             input_west  = 1,
             outputs_east = outs }
  where
    (j, (_, _, outs)) = (runState jnt) (env, 2, [])

mkBox :: Command -> Supply Box
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

mkBoxGrp w n e s grp =
    return $ JBox_Group { boxes = grp,
                          b_north = n,
                          b_south = s,
                          b_east  = e,
                          b_west  = w }

join_ew :: Box -> Box -> Supply Box
join_ew b1 b2 = do
  wire <- return $ b_east b1
  b2' <- return $ b2 { west = wire }
  mkBoxGrp (b_west b1) (north b2) (east b2) (south b2) [b1, b2']

join :: Box -> Box -> Box -> Supply Box
join wi ni box = do
  wire1 <- return $ b_east wi
  wire2 <- return $ b_north ni
  box' <- return $ box { north = wire2,
                         west = wire1 }
  mkBoxGrp (b_west wi) (b_north ni) (east box') (south box') [wi, ni, box']

