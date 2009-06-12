module Twodee.AstExplicit (WireInfo(..),
                           ExplicitOrder(..),
                           liveness_analyze,
                           explicit_wiring)
where

import Twodee.Ast
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import Data.Monoid
import Data.Maybe (fromJust)


-- This datatype explicitly tells what kind of wire we are working with
data WireInfo = PassThrough Int
              | End_W Int
              | End_N Int
              | Start_E Int
              | Start_S Int
  deriving (Eq, Ord)

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
    findEdges' ((id, box) : rest) =
      let
        getOutEdges bx = Set.fromList [south bx, west bx]
        getInEdges bx = Set.fromList [north bx, east bx]
        s = getOutEdges box
        matches boxes = fil boxsets
            where boxsets = fmap (\(id, b) -> (id, getInEdges b)) boxes
                  fil :: [(Int, Set.Set Int)] -> [Int]
                  fil [] = []
                  fil ((tid, bs) : r) = if Set.intersection s bs /= Set.empty
                                        then tid : (fil r)
                                        else fil r
      in
        ([(id, tid) | tid <- matches rest] : findEdges' rest)

-- Sort a list of boxes topologically
topsort :: [Box] -> [Box]
topsort jnts =
    let
        numbered = zip [1..] jnts
        bnds = (1, length numbered)
        edges = findEdges numbered
        vertices = Graph.topSort $ Graph.buildG bnds edges

        order [] numbered = []
        order (v : vs) numbered =
            (fromJust $ lookup v numbered) : (order vs numbered)
    in
      order vertices numbered

-- Helper function. Updated liveness w.r.t. a gen/kill set.
update_liveness genset killset liveelements =
    let
        remove [] es = es
        remove (e : rest) es = filter (\(w, _) -> w /= e) es
        max_num = foldl1 max $ fmap snd liveelements
        add n [] es = es
        add n (e : rest) es = add (n+1) rest ((e, n+1) : es)
    in
      ((add max_num genset) . (remove killset)) liveelements

-- Analyze liveness for an explicitly ordered representation
liveness_analyze :: [(Wire, Int)] -> [ExplicitOrder] -> [ExplicitOrder]
liveness_analyze l [] = []
liveness_analyze l (b : rest) = b {live = updated_live } : (liveness_analyze updated_live rest)
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
      updated_live = update_liveness gen kill l
