module Layout (graphFromProgram,
               testProg)
where

import qualified Data.Sequence as S

import Data.Graph
import Data.List
import Data.Maybe (catMaybes)

import Ast

testProg = [
 MKBox {cmd = Send2 {c_exp0 = (Iface W), out1 = S,
                     c_exp1 = (Iface W), out2 = E},
        north = Nothing, west = Nothing, east = Just 3, south = Just 2},

 MKBox {cmd = Case {c_exp = (Iface N), out1 = S, out2 = E},
        north = Nothing, west = Nothing, east = Just 5, south = Just 4},

 MKBox {cmd = Use { name = "plus" },
        north = Just 4, west = Just 2,  east = Just 6, south = Nothing},

 MKBox {cmd = Send1 {c_exp = Inl {e_exp = (Iface W)}, out = E},
        north = Just 3, west = Just 6, east = Nothing, south = Nothing},

 MKBox {cmd = Send1 {c_exp = (Iface N), out = E},
        north = Nothing, west = Just 5, east = Nothing, south = Nothing}

 ]


type Layout = S.Seq (S.Seq Char)

{-
endBox :: [Box] -> Wire -> Box
endBox boxs wire =
    case find (\box -> ((north box) == wire) || ((west box) == wire)) boxs of
      Nothing  -> error "No such element!"
      Just box -> box
-}

get_edges :: [Box] -> [(Int, Int, [Int])]
get_edges boxes = get_edges' (zip boxes [0..]) [] -- Attach a counter to each box
  where
    -- Find the endpoint of a wire
    find_endpoint wire = findIndex (\box -> ((north box) == wire) || ((west box) == wire)) boxes
    -- Worker; walk the boxes, collecting the edges
    get_edges' []             es = es
    get_edges' ((box, idx) : tail) es =
        let e = (east  box)
            s = (south box)
            edges = catMaybes [(find_endpoint e), (find_endpoint s)]
        in
          get_edges' tail ((idx, idx, edges) : es)

graphFromProgram prg =
    graphFromEdges (get_edges prg)

layoutProgram prg =
    let (graph, lookupN, lookupV) = graphFromProgram prg
        seq = S.fromList $ take (length prg) (repeat 0) :: S.Seq Int
    in
      layoutVertices (map (\x -> (prg !! x)) (reverse (topSort graph)))

layoutVertices lst =
    let len = length lst
        emptys = take len $ repeat $ S.fromList []
    in
      layoutVertices' (S.fromList emptys) lst lst

layoutVertices' :: Layout -> [Box] -> [Box] -> Layout
layoutVertices' lo boxs []           = lo
layoutVertices' lo boxs (box : tail) = lo


connect :: Layout -> (Int, Outface) -> (Int, Inface) -> Int -> Layout
connect lo (idxO, facO) (idxI, facI) height =
    let lo' = map (\x -> x S.>< (S.fromList
                               (take (height - (S.length x)) (repeat ' ')))) in
    lo
