module Layout (graphFromProgram, testProg) where

import Data.Sequence (Seq, fromList)
import Data.List
import Data.Graph
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


type Layout = Seq (Seq Char)



endBox :: [Box] -> Wire -> Box
endBox boxs wire = 
    case find (\box -> ((north box) == wire) || ((west box) == wire)) boxs of 
      Nothing  -> error "No such element!"
      Just box -> box

endBoxIdx :: [Box] -> Wire -> Int
endBoxIdx boxs wire = 
    case findIndex (\box -> ((north box) == wire) || ((west box) == wire)) boxs of 
      Nothing  -> error "No such element!"
      Just i   -> i



get_edge :: [Box] -> Wire -> [Int]
get_edge lst Nothing     = []
get_edge lst i'@(Just i) = [endBoxIdx lst i']

get_edges' :: [Box] -> [Box] -> Int -> [(Int, Int, [Int])] -> [(Int, Int, [Int])]
get_edges' []           lst idx es = es
get_edges' (box : tail) lst idx es = 
    let e = (east  box)
        s = (south box) in 
    let edges = (get_edge lst e) ++ 
                (get_edge lst s) in 
    get_edges' tail lst (idx+1) ((idx, idx, edges) : es)

get_edges :: [Box] -> [(Int, Int, [Int])]
get_edges lst = 
    get_edges' lst lst 0 []


graphFromProgram prg = 
    graphFromEdges (get_edges prg)


layoutProgram prg = 
    let (graph, lookupN, lookupV) = graphFromProgram prg in 
    let seq = fromList $ take (length prg) (repeat 0) :: Seq Int in
    layoutVertices (map (\x -> (prg !! x)) (reverse (topSort graph)))


layoutVertices lst = 
    let lo = (fromList $ take (length lst) 
                           (repeat (fromList []))) in
    layoutVertices' lo lst lst 


layoutVertices' :: Layout -> [Box] -> [Box] -> Layout
layoutVertices' lo boxs [] = lo
layoutVertices' lo boxs (box : tail) = lo
