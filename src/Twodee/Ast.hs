-- Abstract Syntax tree
module Twodee.Ast (Inface (..),
                   Outface (..),
                   Exp (..),
                   Command (..),
                   Box (..),
                   Mod (..),
                   width,
                   boxify)
where

import Data.Monoid

data Inface = N | W
  deriving Show

data Outface = S | E
  deriving Show


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
          Use s -> mconcat ["use \"", show s, "\""]

width :: Command -> Int
width c =
    (2+) $ length $ show c

hRule w = mconcat ["+", take (w-2) $ repeat '-', "+"]

boxify :: Command -> String
boxify c = unlines [rule, sorround "!" $ show c, rule]
  where
    rule = hRule $ width c
    sorround elem str = mconcat [elem, str, elem]


newtype Box = MkBox { unBox :: Inface -> Inface -> Command }

newtype Mod = MkModule { boxes :: [Box] }
