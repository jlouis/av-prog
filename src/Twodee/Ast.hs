-- Abstract Syntax tree
module Twodee.Ast (Inface (..),
                   Outface (..),
                   Exp (..),
                   Command (..),
                   Mod (..),
                   Joint (..),
                   Wire,
                   width)
where

import qualified Data.Graph as Graph
import qualified Data.Set as Set
import Data.List

import Data.Monoid

data Inface = N | W
  deriving Show

data Outface = S | E
  deriving Show

type Wire = Int

data Exp = Unit
         | Tuple Exp Exp
         | Inl   Exp
         | Inr   Exp
         | Iface Inface

instance Show Exp where
    show x =
        case x of
          Unit -> "()"
          Tuple e1 e2 -> mconcat ["(", show e1, ", ", show e2, ")"]
          Inl e -> mconcat ["inl ", show e]
          Inr e -> mconcat ["inr ", show e]
          Iface i -> show i

data Command = SendEmpty
             | Send1 Exp Outface
             | Send2 Exp Outface
                     Exp Outface
             | Case  Exp Outface Outface
             | Split Exp
             | Use   String

instance Show Command where
    show x =
        case x of
          SendEmpty -> "send[]"
          Send1 e o -> mconcat ["send[", show e, ", ", show o, "]"]
          Send2 e1 o1 e2 o2 -> mconcat ["send[",
                                        show e1, ", ", show o1, "]",
                                        "[", show e2, ", ", show o2, "]"]
          Case e1 o1 o2 -> mconcat ["case(", show e1, ", ", show o1,
                                                      ", ", show o2, ")"]
          Split e -> mconcat ["split(", show e, ")"]
          Use s -> mconcat ["use ", show s, ""]

width :: Joint -> Int
width c =
    (2+) $ length $ show $ command c

hRule corner line w = mconcat [corner, take (w-2) $ repeat line, corner]
boxRule = hRule "*" '='
modRule = hRule "," '.'

sorround :: String -> String -> String
sorround elem str = mconcat [elem, str, elem]

data Joint = JBox { command :: Command,
                    north :: Wire,
                    east :: Wire,
                    south :: Wire,
                    west :: Wire }
           | JBox_Group { boxes :: [Joint],
                          b_north :: Wire,
                          b_south :: Wire,
                          b_east  :: Wire,
                          b_west  :: Wire }
           | JSpacing

extract_boxes :: Joint -> [Joint]
extract_boxes jnt =
    case jnt of
      b @ (JBox _ _ _ _ _) -> [b]
      JBox_Group boxes _ _ _ _ -> mconcat $ fmap extract_boxes boxes
      JSpacing -> []

findEdges :: [(Int, Joint)] -> [[(Int, Int)]]
findEdges [] = []
findEdges ((id, box) : rest) =
    let
        getEdges bx = Set.fromList [north bx, east bx, south bx, west bx]
        s = getEdges box
        matches boxes = fil boxsets
            where boxsets = fmap (\(id, b) -> (id, getEdges b)) boxes
                  fil :: [(Int, Set.Set Int)] -> [Int]
                  fil [] = []
                  fil ((tid, bs) : r) = if Set.intersection s bs /= Set.empty
                                        then tid : (fil r)
                                        else fil r
    in
      ([(id, tid) | tid <- matches rest] : findEdges rest)

order [] numbered = []
order (v : vs) numbered =
    case lookup v numbered of
      Just b -> b : (order vs numbered)

topsort jnts =
    let
        numbered = zip [1..] jnts
        bnds = (1, length numbered)
        edges = findEdges numbered
        vertices = Graph.topSort $ Graph.buildG bnds $ mconcat edges
    in
      order vertices numbered

data WireInfo = PassThrough Int
              | End_W Int
              | End_N Int
              | Start_E Int
              | Start_S Int

data ExplicitOrder = EOB { contents :: Command,
                           wires :: [WireInfo] }

process_box :: Joint -> ExplicitOrder
process_box box = EOB { contents = command box,
                        wires = wi }
    where
      wi = [End_W $ west box,
            End_N $ north box,
            Start_E $ east box,
            Start_S $ south box]

explicit_wiring :: [Joint] -> [ExplicitOrder]
explicit_wiring jnts = fmap process_box ordered
  where
    ordered = topsort jnts

instance Show Joint where
    show b =
        show $ (command b)

data Mod = MkModule { joint :: [Joint],
                      modName :: String }

boxRulers :: [Joint] -> [String]
boxRulers [] = [""]
boxRulers (JSpacing : rest) = (take 2 $ repeat ' ') : boxRulers rest
boxRulers (b : rest) = (boxRule $ width b) : boxRulers rest

content :: [Joint] -> [String]
content [] = []
content (JSpacing : rest) = "->" : content rest
content (b : rest) = (sorround "!" $ show b) : content rest

outputJoints layout = present layout
        where
          present l = mconcat $ intersperse "\n" [mconcat $ boxRulers l,
                                                  mconcat $ content l,
                                                  mconcat $ boxRulers l]

instance Show Mod where
    show m = modularize contents
        where
          contents = outputJoints $ joint m
          modularize contents =
              let
                  ls = lines contents
                  size = foldl1 max $ fmap length ls
              in
                unlines [modRule $ 2 + size,
                         " " ++ modName m,
                         mconcat $ intersperse "\n" $ fmap (sorround ":") ls,
                         modRule $ 2 + size]



