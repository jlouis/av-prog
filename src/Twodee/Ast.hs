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
                           wires :: [WireInfo],
                           live :: [(Wire, Int)] }

process_box :: Joint -> ExplicitOrder
process_box box = EOB { contents = command box,
                        wires = wi,
                        live = [] }
    where
      wi = [End_W $ west box,
            End_N $ north box,
            Start_E $ east box,
            Start_S $ south box]

explicit_wiring :: [Joint] -> [ExplicitOrder]
explicit_wiring jnts = fmap process_box ordered
  where
    ordered = topsort jnts

update_liveness genset killset liveelements =
    let
        remove [] es = es
        remove (e : rest) es = filter (\(w, _) -> w /= e) es
        max_num = foldl1 max $ fmap snd liveelements
        add n [] es = es
        add n (e : rest) es = add (n+1) rest ((e, n+1) : es)
    in
      ((add max_num genset) . (remove killset)) liveelements


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

extraSign :: Int -> String  -> String
extraSign 0 string = ""
extraSign number string = string ++ extraSign (number - 1) string

outputJoints layout = present layout
        where
          present l = mconcat $ intersperse "\n" [mconcat $ boxRulers l,
                                                  mconcat $ content l,
                                                  mconcat $ boxRulers l]

width :: Joint -> Int
width c =
    (2+) $ length $ show $ command c

hRule corner line w = mconcat [corner, take (w-2) $ repeat line, corner]
boxRule = hRule "*" '='
modRule = hRule "," '.'

sorround :: String -> String -> String
sorround elem str = mconcat [elem, str, elem]

crate_width :: Bool -> Command -> Int
crate_width west_input command =
    (5+) $ length $ show command

build = intersperse "\n"

has_north_input crate = case find (\e -> case e of
                                      End_N _ -> True
                                      _ -> False) $ wires crate of
                          Nothing -> False
                          Just _ -> True

has_south_output crate = case find (\e -> case e of
                                       Start_S _ -> True
                                       _ -> False) $ wires crate of
                           Nothing -> False
                           Just _ -> True

has_east_output crate = case find (\e -> case e of
                                      Start_E _ -> True
                                      _ -> False) $ wires crate of
                          Nothing -> False
                          Just _ -> True

has_west_input crate = case find (\e -> case e of
                                     End_W _ -> True
                                     _ -> False) $ wires crate of
                         Nothing -> False
                         Just _ -> True

create_north_input1 :: Bool -> Int -> String
create_north_input1 True cw = mconcat ["  ++", take (cw-4) (repeat ' ')]
create_north_input1 False cw = mconcat $ take cw (repeat " ")
create_north_input2 True cw = mconcat [" ++v", take (cw-4) (repeat ' ')]
create_north_input2 False cw = mconcat $ take cw (repeat " ")

create_box_hrule_upper ni cw =
    mconcat ["  ",
             if ni then "|*" else " *",
             mconcat $ take (cw-5) (repeat " "),
             "*"]


create_box_hrule_lower wi ni cw =
    mconcat [if wi then "|" else " ",
             if ni then "|*" else " *",
             mconcat $ take (cw-4) (repeat " "),
             "*"]

create_box_contents wi ni c =
    mconcat [if wi then
                 if ni
                 then "+#>!"
                 else "+->!"
             else "   !", show c, "!"]

create_lines c = []

renderbox :: ExplicitOrder -> [String]
renderbox crate =
    let
        north_input = has_north_input crate
        west_input  = has_west_input crate
        east_output = has_east_output crate
        south_output = has_south_output crate
        cw = crate_width west_input (ctnts)
        ctnts = contents crate
        circuitry = wires crate
        positions = live create
    in
      [create_north_input1 north_input cw,
       create_north_input2 north_input cw,
       create_box_hrule_upper north_input cw,
       create_box_contents west_input north_input ctnts,
       create_box_hrule_lower west_input north_input cw] ++
       create_lines positions circuitry

render :: [ExplicitOrder] -> [[String]]
render boxes = fmap renderbox analyzed_boxes
    where analyzed_boxes = liveness_analyze [] boxes

instance Show Mod where
    show m = modularize contents
        where
          contents = outputJoints $ joint m
          modularize contents =
              let
                  ls = lines contents
                  size = foldl1 max $ fmap length ls
                  name = ": " ++ modName m
                  modLength = size - length name + 1
                  finalName = name ++ (extraSign modLength " ") ++ ":"
              in
                unlines [modRule $ 2 + size,
                         finalName,
                         mconcat $ intersperse "\n" $ fmap (sorround ":") ls,
                         modRule $ 2 + size]



