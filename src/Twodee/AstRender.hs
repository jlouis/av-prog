-- Rendering of Abstract Syntax Trees
module Twodee.AstRender (render)
where

import Twodee.Ast
import Twodee.AstExplicit

import Data.Monoid
import Data.List
import Data.Maybe (fromJust, catMaybes)

crate_width :: Command -> Int
crate_width cmd = length $ show cmd

has_north_input :: ExplicitOrder -> Bool
has_north_input crate = case find (\e -> case e of
                                      End_N _ -> True
                                      _ -> False) $ wires crate of
                          Nothing -> False
                          Just _ -> True

has_south_output :: ExplicitOrder -> Bool
has_south_output crate = case find (\e -> case e of
                                       Start_S _ -> True
                                       _ -> False) $ wires crate of
                           Nothing -> False
                           Just _ -> True

has_east_output :: ExplicitOrder -> Bool
has_east_output crate = case find (\e -> case e of
                                      Start_E _ -> True
                                      _ -> False) $ wires crate of
                          Nothing -> False
                          Just _ -> True

has_west_input :: ExplicitOrder -> Bool
has_west_input crate = case find (\e -> case e of
                                     End_W _ -> True
                                     _ -> False) $ wires crate of
                         Nothing -> False
                         Just _ -> True

fillline :: String -> Int -> String
fillline char k = mconcat $ take k (repeat char)

spaces :: Int -> String
spaces = fillline " "

modhrule :: Int -> String
modhrule = fillline "."

boxhrule :: Int -> String
boxhrule = fillline "="

wireline :: Int -> String
wireline = fillline "-"

line0 :: Int -> String
line0 w = modhrule (w+7)

line1 :: Int -> String
line1 w = spaces (w+7)

line2 :: Bool -> Int -> String
line2 n cw = mconcat ["   ", if n then "++" else "  ", spaces (cw+2)]

line3 :: Bool -> Int -> String
line3 n cw = mconcat [" ", if n then "+-+v" else "    ", spaces (cw+2)]

line4 :: Bool -> Int -> String
line4 n cw = mconcat [if n then " | *" else "   *", boxhrule cw, "*  "]

line5 :: Bool -> Bool -> Bool -> Command -> String
line5 n e w c = mconcat [if w then "+" else " ",
                         if w then
                             if n then "#>" else "->"
                         else "  ",
                         "!", show c, "!",
                         if e then "-+" else "  "]

line6 :: Bool -> Bool -> Bool -> Int -> String
line6 n e w cw = mconcat [if w then "|" else " ",
                         if n then "|" else " ",
                         " ",
                         "*", boxhrule cw, "* ",
                         if e then "|" else " "]

line7 :: Bool -> Bool -> Bool -> Bool -> Int -> String
line7 n e w s cw = mconcat [if w then "|" else " ",
                            if n then "|" else " ",
                            spaces (cw+1),
                            if s then "+-+" else "   ",
                            if e then "|" else " "]

order_wires :: [WireInfo] -> [(Wire, Int)] -> [(Int, [WireInfo])]
order_wires wrs positions =
    collect $ groupBy (\(p1, _) -> \(p2, _) -> p1 == p2) $ sort poslist
        where
          collect [] = []
          collect (elm : rest) =
              (fst $ head elm, fmap snd elm) : collect rest
          poslist :: [(Int, WireInfo)]
          poslist = fmap findpos wrs
          findpos wire =
              (fromJust $ lookup (wirenum wire) positions, wire)

create_lines :: Int -> [WireInfo] -> [(Wire, Int)] -> Bool -> Bool -> Bool -> Bool -> [String]
create_lines cw wrs p n1 e1 w1 s1 =
    let
        ordered_wires = order_wires wrs p
        process_wire [] _ _ _ _ accum = reverse accum
        process_wire (wire : rest) n e w s accum =
            case snd wire of
              PassThrough _ -> process_wire rest n e w s (line : accum)
                  where
                    line = mconcat [if w then "#" else "-",
                                    if n then "#" else "-",
                                    wireline (cw + 3),
                                    if s then "#" else "-",
                                    if e then "#" else "-"]
              End_W _ -> process_wire rest n e False s (line : accum)
                  where
                    line = mconcat ["+", if n then "|" else " ",
                                    spaces (cw+3),
                                    if s then "|" else " ",
                                    if e then "|" else " "]
              Start_S _ -> process_wire rest n e w False (line : accum)
                  where
                    line = mconcat [if w then "|" else " ",
                                    if n then "|" else " ",
                                    spaces (cw+3),
                                    "+",
                                    if e then "#" else "-"]
              Start_E _ -> process_wire rest n False w s (line : accum)
                  where
                    line = mconcat [if w then "|" else " ",
                                    if n then "|" else " ",
                                    spaces (cw+3),
                                    if s then "|" else " ",
                                    "+"]
              End_N _ -> process_wire rest False e w s (line : accum)
                  where
                    line = mconcat [if w then "#" else "-",
                                    "+",
                                    spaces (cw+3),
                                    if s then "|" else " ",
                                    if e then "|" else " "]
    in
      []
--      process_wire ordered_wires n1 e1 w1 s1 []

renderbox :: ExplicitOrder -> Maybe [String]
renderbox (EOM _) = Nothing
renderbox (crate@(EOB _ _ _)) =
    let
        n = has_north_input crate
        w = has_west_input crate
        e = has_east_output crate
        s = has_south_output crate
        cw = crate_width ctnts
        ctnts = contents crate
        circuitry = wires crate
        positions = live crate
    in
      Just $ [line0 cw,
              line1 cw,
              line2 n cw,
              line3 n cw,
              line4 n cw,
              line5 n e w ctnts,
              line6 n e w cw,
              line7 n e w s cw] ++
             create_lines cw circuitry positions n e w s

render_eo :: [ExplicitOrder] -> [String]
render_eo bxs = join $ catMaybes $ fmap renderbox analyzed_boxes
    where analyzed_boxes = liveness_analyze [] [] bxs
          join x = fmap mconcat $ transpose x

create_module_boxes :: Wire -> Wire -> [Wire] -> [ExplicitOrder]
create_module_boxes inp_n inp_w out_e = [start_box, end_box]
    where
      -- Map the Start_S to the north input and the Start_E to the west input
      start_box = EOM { wires = [Start_S inp_n, Start_E inp_w] }
      end_box   = EOM { wires = fmap End_W out_e }

render_module :: Mod -> String
render_module (Module bxs nam inp_n inp_w out_e) =
    let
        boxs = extract_base_box bxs
        [start_box, end_box] = create_module_boxes inp_n inp_w out_e
        eobs = explicit_wiring boxs
        rendered = render_eo ([start_box] ++ eobs ++ [end_box])
        module_width = foldl1 max $ fmap length rendered
        name_width = length nam
        north_input = False -- TODO: Fix me.
        l0 = mconcat [",", modhrule (name_width + module_width + 2),","]
        l1 = mconcat [":", nam, " ", if north_input then "|" else " ",
                         spaces (module_width), ":"]
        -- Add the line here to get a west input
        -- Add the lines here to start connecting via the rendered lines
        -- Add the lines here to add exits from the box
        lf = mconcat [",", modhrule (name_width + module_width + 2), ","]

    in
      mconcat [l0, l1, lf]

render :: [Mod] -> String -> String
render modules stdlib = mconcat [rendered_mods, stdlib]
  where rendered_mods = mconcat $ fmap render_module modules
