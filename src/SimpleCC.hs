module SimpleCC (compile)
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
type FEnvironment = [(String, Ast, VEnvironment)]
type Inputs = [Wire]
type Supply a = State (VEnvironment, Int, [Int], Inputs) a

new :: Supply Int
new = do
  (env, count, outlist, ins) <- get
  put (env, count+1, outlist, ins)
  return count

getInput :: Supply Int
getInput = do
  (env, count, outlist, h : ins) <- get
  put (env, count+1, outlist, ins)
  return h

putInputs :: [Int] -> Supply ()
putInputs inps = do
  (env, count, outlist, _) <- get
  put (env, count, outlist, inps)

env_to_list :: EnvValue -> VEnvironment
env_to_list EnvEnd = []
env_to_list (Env (Const str ast) rest) =
    (str, ast) : (env_to_list rest)

fkt_to_list :: FuncEnvValue -> FEnvironment
fkt_to_list FuncEnd = []
fkt_to_list (FuncEnv (Func nam ast env) rest) =
    (nam, ast, env_to_list env) : (fkt_to_list rest)

envL str = do
  (env, _, _, _) <- get
  return $ fromJust $ lookup str env

countInputs :: Ast -> Int
countInputs x =
    case x of
      Zero -> 0
      Succ x -> countInputs x
      Plus e1 e2 -> (countInputs e1) + (countInputs e2)
      Mul e1 e2  -> (countInputs e1) + (countInputs e2)
      Lookup str -> if str == "input" then 1 else 0
      Call _ ast -> countInputs ast
      Contz t z s -> sum $ fmap countInputs [t, z, s]
      Pre _ -> 0

mkInputGrp :: Ast -> Supply ([Box], [Wire])
mkInputGrp ast = build_split n [] [] mod_in_w
    where
      split earlier_wire =
          do
            b <- mkBox (Split (Tuple (Iface W) (Iface W)))
            b' <- return $ b { west = earlier_wire }
            return $ (b', south b', east b')
      build_split 0 boxes out_wires _ = return (boxes, out_wires)
      build_split n boxes out_wires next_wire =
          do
            (newbox, output, next') <- split next_wire
            build_split (n-1) (newbox : boxes) (output : out_wires) next'
      n = countInputs ast

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
    if s == "input"
    then
        do
          input <- getInput
          b <- mkBox (Send1 (Iface W) E)
          return $ b { west = input }
    else
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
      wire_e <- return $ east grp
      wire_s <- return $ south grp
      ccz' <- return $ ccz { west = wire_s }
      ccs' <- return $ ccs { west = wire_e }
      mkBoxGrp (west c1) (north c1) (east ccz') (south ccs')
               [c1, ccz, ccs, b]
compileE (Call str ast) =
    do
      cx <- compileE ast
      b <- mkBox (Use str)
      join_ew cx b

compileF :: (String, Ast, VEnvironment) -> Mod
compileF (nam, ast, env) = mkModule nam ret env
  where
    ret = do
      (boxes, inputs) <- mkInputGrp ast
      putInputs inputs
      x <- compileE ast
      output_e x
      assemble boxes x

compile :: Ast -> EnvValue -> FuncEnvValue -> [Mod]
compile ast env funcs =
    let
        fenv = fkt_to_list funcs
        venv = env_to_list env
        compiled_funcs = fmap compileF fenv
    in
      compiled_funcs ++ [(compileF ("main", ast, venv))]

----------------------------------------------------------------------
-- Helpers for box manipulation
----------------------------------------------------------------------

{-
mod_in_n :: Int
mod_in_n = 0
-}
mod_in_w :: Int
mod_in_w = 1

assemble :: [Box] -> Box -> Supply Box
assemble adds b = return assembled
    where
      assembled = b { boxes = bs ++ adds }
      bs = boxes b

output_e x =
    do
      case x of
        JBox _ _ _ _ _ -> output $east x
        JBox_Group _ _ _ _ _ -> output $ east x
      return x

output w = do
  (env, count, outs, ins) <- get
  put (env, count, w : outs, ins)

mkModule :: String -> Supply Box -> VEnvironment -> Mod
mkModule n jnt env =
    Module { mod_boxes = [j],
             name = n,
             input_north = 0,
             input_west  = 1,
             outputs_east = outs }
  where
    (j, (_, _, outs, _)) = (runState jnt) (env, 2, [], [])

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
                          north = n,
                          south = s,
                          east  = e,
                          west  = w }

join_ew :: Box -> Box -> Supply Box
join_ew b1 b2 = do
  wire <- return $ east b1
  b2' <- return $ b2 { west = wire }
  mkBoxGrp (west b1) (north b2) (east b2) (south b2) [b1, b2']

join :: Box -> Box -> Box -> Supply Box
join wi ni box = do
  wire1 <- return $ east wi
  wire2 <- return $ north ni
  box' <- return $ box { north = wire2,
                         west = wire1 }
  mkBoxGrp (west wi) (north ni) (east box') (south box') [wi, ni, box']

