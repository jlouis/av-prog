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

import Data.List
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
          Use s -> mconcat ["use ", show s, ""]

width :: Box Inface -> Int
width c =
    (2+) $ length $ show $ unBox c W N

hRule corner line w = mconcat [corner, take (w-2) $ repeat line, corner]
boxRule = hRule "*" '='
modRule = hRule "," '.'

sorround :: String -> String -> String
sorround elem str = mconcat [elem, str, elem]

newtype Box a = MkBox { unBox :: a -> a -> Command }

instance Show (Box Inface) where
    show b =
        show $ (unBox b) W N

data Joint = JBox (Box Inface)
           | JSpacing -- Need more attachments here
data Mod = MkModule { boxes :: [Joint],
                      modName :: String }

boxRulers :: [Joint] -> [String]
boxRulers [] = [""]
boxRulers (JSpacing : rest) = (take 2 $ repeat ' ') : boxRulers rest
boxRulers (JBox b : rest) = (boxRule $ width b) : boxRulers rest

contents :: [Joint] -> [String]
contents [] = []
contents (JSpacing : rest) = "->" : contents rest
contents (JBox b : rest)   = (sorround "!" $ show b) : contents rest

outputJoints layout = present layout
        where
          present l = mconcat $ intersperse "\n" [mconcat $ boxRulers l, mconcat $ contents l, mconcat $ boxRulers l]

instance Show Mod where
    show m = modularize contents
        where
          contents = outputJoints $ boxes m
          modularize contents =
              let
                  ls = lines contents
                  size = foldl1 max $ fmap length ls
              in
                unlines [modRule $ 2 + size,
                         " " ++ modName m,
                         mconcat $ intersperse "\n" $ fmap (sorround ":") ls,
                         modRule $ 2 + size]



