module Twodee.Layout ()
where

import qualified Data.Sequence as S

import Data.Foldable
import Data.Graph
import Data.List
import Data.Maybe (catMaybes)

import Twodee.Ast


{-
testProg = [
 MkBox {cmd = Send2 {c_exp0 = (Iface W), out1 = S,
                     c_exp1 = (Iface W), out2 = E},
        north = Nothing, west = Nothing, east = Just 3, south = Just 2},

 MkBox {cmd = Case {c_exp = (Iface N), out1 = S, out2 = E},
        north = Nothing, west = Nothing, east = Just 5, south = Just 4},

 MkBox {cmd = Use { name = "plus" },
        north = Just 4, west = Just 2,  east = Just 6, south = Nothing},

 MkBox {cmd = Send1 {c_exp = Inl (Iface W)}, out = E},
        north = Just 3, west = Just 6, east = Nothing, south = Nothing},

 MkBox {cmd = Send1 (Iface N), out = E},
        north = Nothing, west = Just 5, east = Nothing, south = Nothing}

 ]
-}

newtype  Layout = MKLayout ( S.Seq (S.Seq Char, Char, S.Seq Char) )
instance Show Layout where
    show lo = (showHigh lo) ++ (showMid lo) ++ (showLow lo)

showLow :: Layout -> String
showLow (MKLayout lo) =
    map (\(x,y) -> (\(l,m,h) -> S.index l y) (S.index lo x))
            [(x,y) | x <- [1..S.length lo], y <- [1..((\(l,m,h) -> S.length l) (S.index lo 0))]]

showMid :: Layout -> String
showMid (MKLayout lo) =
    map (\(x) -> (\(l,m,h) -> m) (S.index lo x)) [(x) | x <- [1..S.length lo]]

showHigh :: Layout -> String
showHigh (MKLayout lo) =
    map (\(x,y) -> (\(l,m,h) -> S.index h y) (S.index lo x))
            [(x,y) | x <- [1..(S.length lo)-1], y <- [1..((\(l,m,h) -> (S.length l)-1) (S.index lo 0))]]



c_BLOCK_SIZE = 4

{-
endBox :: [Box] -> Wire -> Box
endBox boxs wire =
    case find (\box -> ((north box) == wire) || ((west box) == wire)) boxs of
      Nothing  -> error "No such element!"
      Just box -> box
-}


-- Find the endpoint of a wire
{-
find_endpoint wire boxes = findIndex (\box -> ((north box) == wire) || ((west box) == wire)) boxes

get_edges :: [Box] -> [(Int, Int, [Int])]
get_edges boxes = map get_edges (zip boxes [0..]) -- Attach a counter to each box
    -- Worker; collect the outbound edges of a box
    where
      get_edges (box, idx) =
          let e = (east  box)
              s = (south box)
              edges = catMaybes [(find_endpoint e boxes), (find_endpoint s boxes)]
          in
            (idx, idx, edges)


graphFromProgram prg =
    graphFromEdges (get_edges prg)


layoutProgram :: [Box] -> Layout
layoutProgram prg =
    let (graph, lookupN, lookupV) = graphFromProgram prg
        seq = S.fromList $ take (length prg) (repeat 0) :: S.Seq Int
    in
      layoutVertices (map (\x -> (prg !! x)) (reverse (topSort graph)))


layoutVertices lst =
    let len = (length lst)*c_BLOCK_SIZE in
    let lo = MKLayout (S.fromList (take len $ repeat $ (S.fromList [], 'x', S.fromList []))) in
    layoutVertices' lo 1 lst lst


layoutVertices' :: Layout -> Int -> [Box] -> [Box] -> Layout
layoutVertices' lo height boxs []           = lo
layoutVertices' lo height boxs (box : tail) =
    let e = (east  box)
        s = (south box)
        edges = catMaybes [(find_endpoint e boxs), (find_endpoint s boxs)]
    in
    let lo' = connect lo (0, E) (3, W) height in
    layoutVertices' lo' (height+1) boxs tail


makeVerticalUp   height = S.fromList ((take (height-2) (repeat '|')) ++ ['-'])
makeVerticalDown height = S.reverse  (makeVerticalUp height)


getSeqLine :: (S.Seq Char, Char, S.Seq Char) -> Int -> S.Seq Char
getSeqLine (low, mid, high) 0 = error "requested wrong height"
getSeqLine (low, mid, high) height =
    if height > 0 then high else low


makeHorizon :: Layout -> Int -> Int -> Int -> Layout
makeHorizon (MKLayout lo) height idxFrom idxTo =
    if   idxFrom == idxTo then (MKLayout lo)
    else
         let line    = getSeqLine (S.index lo idxFrom) height in
         let height' = abs height in
         let line'   = S.update height' '-' line
             lo'     = S.adjust (updater line') idxFrom lo
         in
         makeHorizon (MKLayout lo') height' (idxFrom+1) idxTo
    where updater line (l,m,h) =
              if height > 0 then (l,m,line) else (line,m,h)

makeHorizonHigh = makeHorizon
makeHorizonLow lo height = makeHorizon lo (-1*height)



drawLineHigh :: Layout -> Int -> Int -> Int -> Layout
drawLineHigh (MKLayout lo) idxO idxI height =
    let seq' = makeVerticalUp height in
    MKLayout (S.adjust (\(l,m,h) -> (l,'<',seq')) (idxO+1) lo)


drawLineLow :: Layout -> Int -> Int -> Int -> Layout
drawLineLow (MKLayout lo) idxO idxI height =
    let seq' = makeVerticalDown height in
    MKLayout (S.adjust (\(l,m,h) -> (seq','^',h)) idxO lo)


drawFinalLineHigh :: Layout -> Int -> Int -> Inface -> Int -> Layout
drawFinalLineHigh (MKLayout lo) idxO idxI facI height =
    let vertLine = makeVerticalUp (height-1) in
    case facI of
      N ->
          let (l,m,h) = S.index lo idxI in
          let line' = (l, m, (S.fromList ['v']) S.>< vertLine) in
          MKLayout (S.update idxI line' lo)
      W ->
          let (l,m,h) = S.index lo (idxI-1) in
          let line' = ((S.fromList ['>']) S.>< vertLine, m, h) in
          MKLayout (S.update (idxI-1) line' lo)


drawFinalLineLow :: Layout -> Int -> Int -> Inface -> Int -> Layout
drawFinalLineLow (MKLayout lo) idxO idxI facI height =
    let vertLine = makeVerticalDown (height-1) in
    case facI of
      N ->
          let (l,m,h) = S.index lo (idxI-1) in
          let line'   = (l, m, (S.fromList ['|','-']) S.>< vertLine) in
          MKLayout (S.update (idxI-1) line' lo)
      W ->
          let (l,m,h) = S.index lo idxI in
          let line'   = (l, m, (S.fromList ['>']) S.>< vertLine) in
          MKLayout (S.update idxI line' lo)



ensureHeight :: Int -> (S.Seq Char, Char, S.Seq Char) -> (S.Seq Char, Char, S.Seq Char)
ensureHeight height (low, mid, high) =
    let low'  = ensureOne low
        high' = ensureOne high
    in (low', mid, high')
    where ensureOne s =
              s S.>< (S.fromList (take (height - (S.length s)) (repeat ' ')))


connect :: Layout -> (Int, Outface) -> (Int, Inface) -> Int -> Layout
connect (MKLayout lo) (idxO, facO) (idxI, facI) height =
    let lo1 = fmap (ensureHeight height) lo
    in
      -- output -> mid
      let lo2 = case facO of
                  E -> drawLineHigh (MKLayout lo1) idxO idxI height
                  S -> drawLineLow  (MKLayout lo1) idxO idxI height
      in
        -- mid
        let lo3 = makeHorizon lo2 (case facO of E -> height
                                                S -> (-1*height)) idxO idxI in
        -- mid -> input
        case facO of
          E -> drawFinalLineHigh lo3 idxO idxI facI height
          S -> drawFinalLineLow  lo3 idxO idxI facI height
-}