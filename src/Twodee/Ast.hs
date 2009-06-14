-- Abstract Syntax tree
module Twodee.Ast (Inface (..),
                   Outface (..),
                   Exp (..),
                   Command (..),
                   Mod (..),
                   Box (..),
                   Wire,
                   extract_base_box)
where

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

data Command = SendEmpty
             | Send1 Exp Outface
             | Send2 Exp Outface
                     Exp Outface
             | Case  Exp Outface Outface
             | Split Exp
             | Use   String

instance Show Exp where
    show x =
        case x of
          Unit -> "()"
          Tuple e1 e2 -> mconcat ["(", show e1, ", ", show e2, ")"]
          Inl e -> mconcat ["inl ", show e]
          Inr e -> mconcat ["inr ", show e]
          Iface i -> show i

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

-- Boxs are vertices in a graph connecting boxes via wires.
data Box = JBox { command :: Command,
                    north :: Wire,
                    east :: Wire,
                    south :: Wire,
                    west :: Wire }
           | JBox_Group { boxes :: [Box],
                          north :: Wire,
                          south :: Wire,
                          east  :: Wire,
                          west  :: Wire }

data Mod = Module { mod_boxes :: [Box],
                    name :: String,
                    input_north :: Wire,
                    input_west :: Wire,
                    outputs_east :: [Wire] }

-- Simplify the boxs, removing the Groups
extract_base_box :: [Box] -> [Box]
extract_base_box ((JBox_Group bxs _ _ _ _) : rest) = simplified ++ (extract_base_box rest)
    where
      simplified = extract_base_box bxs
extract_base_box (b : rest) = [b] ++ (extract_base_box rest)
extract_base_box [] = []




