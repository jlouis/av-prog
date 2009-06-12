module Twodee.AstExplicit (WireInfo(..),
                           ExplicitOrder(..),
                           liveness_analyze,
                           wirenum,
                           prune,
                           explicit_wiring)
where

import Twodee.Ast
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import qualified Data.List as List
import Data.Monoid
import Data.Maybe (fromJust)


-- This datatype explicitly tells what kind of wire we are working with
data WireInfo = PassThrough Int
              | End_W Int
              | End_N Int
              | Start_E Int
              | Start_S Int
  deriving (Eq, Ord)

wirenum :: WireInfo -> Int
wirenum w =
    case w of
      PassThrough k -> k
      End_W k -> k
      End_N k -> k
      Start_E k -> k
      Start_S k -> k

-- This datatype orders wires explicitly
data ExplicitOrder = EOB { contents :: Command,
                           wires :: [WireInfo],
                           live :: [(Wire, Int)] }
                   | EOM { wires :: [WireInfo] }

-- Convert a box to its explicit representation
process_box :: Box -> ExplicitOrder
process_box box = EOB { contents = command box,
                        wires = wi,
                        live = [] }
    where
      wi = [End_W $ west box,
            End_N $ north box,
            Start_E $ east box,
            Start_S $ south box]

-- Lift process_box via a functor
explicit_wiring :: [Box] -> [ExplicitOrder]
explicit_wiring jnts = fmap process_box ordered
  where
    ordered = topsort jnts

-- Given a list of numbered boxes, find the boxes each box connects to and
-- build up a list of these edges.
-- TODO: Work on explicit representation?
findEdges :: [(Int, Box)] -> [(Int, Int)]
findEdges lst = mconcat $ findEdges' lst
  where
    findEdges' :: [(Int, Box)] -> [[(Int, Int)]]
    findEdges' [] = []
    findEdges' ((sid, box) : rest) =
      let
        getOutEdges bx = Set.fromList [south bx, west bx]
        getInEdges bx = Set.fromList [north bx, east bx]
        s = getOutEdges box
        matches bxs = fil boxsets
            where boxsets = fmap (\(iden, b) -> (iden, getInEdges b)) bxs
                  fil :: [(Int, Set.Set Int)] -> [Int]
                  fil [] = []
                  fil ((tid, bs) : r) = if Set.intersection s bs /= Set.empty
                                        then tid : (fil r)
                                        else fil r
      in
        ([(sid, tid) | tid <- matches rest] : findEdges' rest)

-- Sort a list of boxes topologically
topsort :: [Box] -> [Box]
topsort jnts =
    let
        numbered = zip [1..] jnts
        bnds = (1, length numbered)
        edges = findEdges numbered
        vertices = Graph.topSort $ Graph.buildG bnds edges

        order [] = []
        order (v : vs) =
            (fromJust $ lookup v numbered) : (order vs)
    in
      order vertices

-- Helper function. Updated liveness w.r.t. a gen/kill set.
update_liveness :: [Wire] -> [Wire] -> [(Wire, Int)] -> [Int] -> ([(Wire,Int)], [Int])
update_liveness genset killset liveelements freelist =
    let
        remove [] es fl = (es, fl)
        remove (e : rest) es fl =
            let
                culprits = [n | (w, n) <- es, e == w]
            in
              remove rest (filter (\(w, _) -> w /= e) es) (culprits ++ fl)
        -- Gather liveness not containing the numbers we killed
        (killed, freelist') =
            remove killset liveelements freelist
        -- If we need a new position, we can use max_num which is the largest
        -- number amongst the killed
        max_num = foldl1 max $ fmap snd killed
        -- Add generated wires. If we need something, we take it from the freelist
        -- first
        add _ fl [] es = (es, fl)
        add n [] (e : rest) es = add (n+1) [] rest ((e, n+1) : es)
        add n (free : slots) (e : rest) es = add n slots rest ((e, free) : es)
    in
      add max_num freelist' genset killed

-- Analyze liveness for an explicitly ordered representation
liveness_analyze :: [(Wire, Int)] -> [Int] -> [ExplicitOrder] -> [ExplicitOrder]
liveness_analyze _ _ [] = []
liveness_analyze l fl (b : rest) = b {live = updated_live } : (liveness_analyze updated_live fl' rest)
    where
      find_gen [] = []
      find_gen (e : es) =
          case e of
            Start_E i -> i : (find_gen es)
            Start_S i -> i : (find_gen es)
            _ -> find_gen es
      find_kill [] = []
      find_kill (e : es) =
          case e of
            End_W i -> i : (find_kill es)
            End_N i -> i : (find_kill es)
            _ -> find_kill es
      gen = find_gen $ wires b
      kill = find_kill $ wires b
      (updated_live, fl') = update_liveness gen kill l fl

-- Search the wires and prune each wire that points to no-one
-- First a number of helpers are declared. Then they are used.
prune_kill :: [ExplicitOrder] -> Set.Set Int -> [ExplicitOrder]
prune_kill [] _ = []
prune_kill (w@(EOM wrs) : rest) pruneset = w { wires = filtered} : (prune_kill rest pruneset)
  where
    filtered = filter (\wi -> not $ Set.member (wirenum wi) pruneset) wrs
prune_kill (w@(EOB _ wrs _) : rest) pruneset = w { wires = filtered } : (prune_kill rest pruneset)
  where
    filtered = filter (\wi -> not $ Set.member (wirenum wi) pruneset) wrs

search_wire :: Wire -> [ExplicitOrder] -> Bool
search_wire _ [] = False
search_wire wn ((EOB _ wrs _) : rest) =
    if wn `elem` (fmap wirenum wrs)
    then True
    else search_wire wn rest
search_wire wn ((EOM wrs) : rest) =
    if wn `elem` (fmap wirenum wrs)
    then True
    else search_wire wn rest

mkPruneSet :: [ExplicitOrder] -> Set.Set Int -> Set.Set Int
mkPruneSet [] s = s
mkPruneSet (wr : rest) s =
    case wr of
      EOB _ wrs _ -> p wrs
      EOM wrs     -> p wrs
  where
    p wrs = mkPruneSet rest s'
      where
        s' = Set.union s (Set.fromList $ find_dead wrs)
        find_dead [] = []
        find_dead (w : ws) =
            if search_wire (wirenum w) rest
            then wirenum w : find_dead ws
            else find_dead ws

prune :: [ExplicitOrder] -> [ExplicitOrder]
prune eo = prune_kill eo (mkPruneSet eo Set.empty)
