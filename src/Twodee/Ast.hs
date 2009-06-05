-- Abstract Syntax tree
module Twodee.Ast (Inface (..),
                   Outface (..),
                   Exp (..),
                   Command (..),
                   Box (..),
                   Mod (..),
                   Joint (..),
                   width)
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

width :: Box Inface -> Int
width c =
    (2+) $ length $ show $ unBox c W N

hRule w = mconcat ["+", take (w-2) $ repeat '-', "+"]

sorround :: String -> String -> String
sorround elem str = mconcat [elem, str, elem]


newtype Box a = MkBox { unBox :: a -> a -> Command }

instance Show (Box Inface) where
    show b =
        show $ (unBox b) W N

data Joint = JBox (Box Inface)
           | JSpacing -- Need more attachments here

hRulers :: [Joint] -> [String]
hRulers [] = [""]
hRulers (JSpacing : rest) = (take 2 $ repeat ' ') : hRulers rest
hRulers (JBox b : rest) = (hRule $ width b) : hRulers rest

contents :: [Joint] -> [String]
contents [] = []
contents (JSpacing : rest) = "->" : contents rest
contents (JBox b : rest)   = (sorround "!" $ show b) : contents rest

instance Show [Joint] where
    show layout = present layout
        where
          present l = unlines [mconcat $ hRulers l, mconcat $ contents l, mconcat $ hRulers l]

newtype Mod a = MkModule { boxes :: [Box a] }
