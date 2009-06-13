-- Rendering of Abstract Syntax Trees
module Twodee.AstRender (render)
where

import Twodee.Ast
import Twodee.AstExplicit

import Data.Monoid
import Data.List
import Data.Maybe (fromJust)

crate_width :: Command -> Int
crate_width cmd = length $ show cmd

has_north_input :: [WireInfo] -> Bool
has_north_input wires = case find (\e -> case e of
                                      End_N _ -> True
                                      _ -> False) $ wires of
                          Nothing -> False
                          Just _ -> True

has_south_output :: [WireInfo] -> Bool
has_south_output wires = case find (\e -> case e of
                                       Start_S _ -> True
                                       _ -> False) $ wires of
                           Nothing -> False
                           Just _ -> True

has_east_output :: [WireInfo] -> Bool
has_east_output wires = case find (\e -> case e of
                                      Start_E _ -> True
                                      _ -> False) $ wires of
                          Nothing -> False
                          Just _ -> True

has_west_input :: [WireInfo] -> Bool
has_west_input wires = case find (\e -> case e of
                                     End_W _ -> True
                                     _ -> False) $ wires of
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

type Position = [(Wire, Int)]

order_wires :: [WireInfo] -> Position -> [(Int, [WireInfo])]
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

find_end :: [WireInfo] -> Maybe WireInfo
find_end [] = Nothing
find_end ((End_W k) : _) = Just (End_W k)
find_end ((End_N k) : _) = Just (End_N k)
find_end (_ : rest) = find_end rest

find_start :: [WireInfo] -> Maybe WireInfo
find_start [] = Nothing
find_start ((Start_E k) : _) = Just (Start_E k)
find_start ((Start_S k) : _) = Just (Start_S k)
find_start (_ : rest) = find_start rest

find_passthrough :: [WireInfo] -> Maybe WireInfo
find_passthrough [] = Nothing
find_passthrough ((PassThrough k) : _) = Just (PassThrough k)
find_passthrough (_ : rest) = find_passthrough rest

create_lines :: Int -> Int -> [WireInfo] -> Position -> Bool -> Bool -> Bool -> Bool -> [String]
create_lines max_pos cw wrs p n e w s =
    let
        ordered_wires = order_wires wrs p
        process _ _ _ _ k [] accum = reverse
                                     (take (max_pos - k) $ repeat (spaces (cw + 7))
                                               ++ accum)
        process n e w s k ((pos, wrs) : rest) accum =
            if k == max_pos then reverse accum
            else if k < pos then
                     let line = mconcat [if w then "|" else " ",
                                         if n then "|" else " ",
                                         spaces (cw+3),
                                         if s then "|" else " ",
                                         if e then "|" else " "]
                     in
                       process n e w s (k+1) ((pos, wrs) : rest) (line : accum)
                 else
                     let
                         ending = find_end wrs
                         starting = find_start wrs
                         passthrough = find_passthrough wrs

                     in
                       case passthrough of
                         Just (PassThrough k) ->
                             let line = mconcat [if w then "#" else "-",
                                                 if n then "#" else "-",
                                                 wireline (cw + 3),
                                                 if s then "#" else "-",
                                                 if e then "#" else "-"]
                             in
                               process n e w s (k+1) rest (line : accum)
                         Nothing ->
                             let
                                 start_part n w =
                                     case ending of
                                       Just (End_W _) ->
                                           ["+", if n then "|" else " "]
                                       Just (End_N _) ->
                                           [if w then "#" else "-", "+"]
                                       Nothing ->
                                           [if w then "|" else " ",
                                            if n then "|" else " "]
                                       _ -> error "Impossible"
                                 end_part s e =
                                     case starting of
                                       Just (Start_S _) ->
                                           ["+", if e then "#" else "-"]
                                       Just (Start_E _) ->
                                           [if s then "|" else " ",
                                            "+"]
                                       Nothing ->
                                           [if s then "|" else " ",
                                            if e then "|" else " "]
                                       _ -> error "Impossible"
                                 (n1, e1, w1, s1) =
                                     (case ending of
                                        Just (End_N _) -> False
                                        _ -> n,
                                      case starting of
                                        Just (Start_E _) -> False
                                        _ -> e,
                                      case ending of
                                        Just (End_W _) -> False
                                        _ -> w,
                                      case starting of
                                        Just (Start_S _) -> False
                                        _ -> s)
                                 line =
                                     mconcat [mconcat $ start_part n w,
                                              spaces (cw+3),
                                              mconcat $ end_part s e]
                             in
                               process n1 e1 w1 s1 (k+1) rest (line : accum)
                         _ -> error "Impossible"
    in
      process n e w s 0 ordered_wires []

start_line0 :: String -> Bool -> String
start_line0 name n = mconcat [",", modhrule $ length name + 1,
                              if n then "|" else "."]

start_line1 :: String -> Bool -> String
start_line1 name n = mconcat [":", name,
                              if n then " |" else "  "]

start_line :: String -> Bool -> String
start_line name n = mconcat [":", spaces $ length name + 1,
                             if n then "|" else " "]

create_start_lines :: Bool -> [WireInfo] -> Int -> Position -> Int -> [String]
create_start_lines n wires max_pos p nl =
    let
        ordered_wires = order_wires wires p
        process _ k [] accum = reverse ((++ accum) $ take (max_pos - k) $ repeat line)
            where
              line = mconcat [":", spaces (nl+2)]
        process n k ((pos, wr) : rest) accum =
            if k == max_pos then reverse accum
            else if k < pos then
                     let
                         line = mconcat [":", spaces (nl+1),
                                         if n then "|" else " "]
                     in
                       process n (k+1) ((pos, wr) : rest) (line : accum)
                 else
                     let
                         (line, next) = case wr of
                                          [Start_S _] ->
                                              (mconcat [":", spaces (nl+1), "+"], False)
                                          [Start_E _] ->
                                              (wireline (nl+3), n)
                                          _ -> error "Impossible case"
                     in
                       process next (k+1) rest (line : accum)

    in
      process n 0 ordered_wires []

create_end_lines :: [WireInfo] -> Position -> Int -> [String]
create_end_lines wires p max_pos =
    let
        ordered_wires = order_wires wires p
        process k [] accum = reverse ((++ accum) $ take (max_pos - k) $ repeat ":")
        process k ((pos, wr) : rest) accum =
            if k == max_pos then reverse accum
            else if k < pos then
                     process (k+1) ((pos, wr) : rest) (":" : accum)
                 else
                     process (k+1) rest ("-" : accum)
    in
      process 0 ordered_wires []

renderbox_start :: Int -> Position -> String -> [WireInfo] -> [String]
renderbox_start max_pos pos name wires =
    let
        n = has_north_input wires
    in
       [start_line0 name n,
        start_line1 name n,
        start_line name n, -- 2
        start_line name n,
        start_line name n, -- 4
        start_line name n,
        start_line name n, -- 6
        start_line name n] ++
       create_start_lines n wires max_pos pos (length name) ++
       [start_line0 name n]

renderbox_end :: Int -> Position -> [WireInfo] -> [String]
renderbox_end max_pos pos wires = [",",
                                   ":", ":", ":", -- 3
                                   ":", ":", ":", ":"] ++
                                  create_end_lines wires pos max_pos ++
                                  [","]

renderbox :: Int -> ExplicitOrder -> [String]
renderbox max_pos ((EOM wires name ty liveness)) =
    case ty of
      StartMod -> renderbox_start max_pos liveness name wires
      EndMod   -> renderbox_end   max_pos liveness wires
renderbox max_pos (crate@(EOB _ _ _)) =
    let
        n = has_north_input $ wires crate
        w = has_west_input $ wires crate
        e = has_east_output $ wires crate
        s = has_south_output $ wires crate
        cw = crate_width ctnts
        ctnts = contents crate
        circuitry = wires crate
        positions = live crate
    in
      [line0 cw,
       line1 cw,
       line2 n cw,
       line3 n cw,
       line4 n cw,
       line5 n e w ctnts,
       line6 n e w cw,
       line7 n e w s cw] ++
      create_lines max_pos cw circuitry positions n e w s ++
       [line0 cw]

render_eo :: [ExplicitOrder] -> [String]
render_eo bxs = join $ fmap (renderbox (freeMax fl)) analyzed_boxes
    where
      (fl, analyzed_boxes) = liveness_analyze [] emptyFreelist bxs []
      join x = fmap mconcat $ transpose x

create_module_boxes :: String -> Wire -> Wire -> [Wire] -> [ExplicitOrder]
create_module_boxes n inp_n inp_w out_e = [start_box, end_box]
    where
      start_box = EOM { wires = [Start_S inp_n, Start_E inp_w],
                        mod_name = n,
                        ty = StartMod,
                        live = []}
      end_box   = EOM { wires = fmap End_W out_e,
                        mod_name = n,
                        ty = EndMod,
                        live = [] }

render_module :: Mod -> String
render_module (Module bxs nam inp_n inp_w out_e) =
    let
        boxs = extract_base_box bxs
        [start_box, end_box] = create_module_boxes nam inp_n inp_w out_e
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
