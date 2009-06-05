-- Abstract Syntax tree
module Twodee.Ast (Inface (..),
                   Outface (..),
                   Exp (..),
                   Command (..),
                   Wire,
                   Box (..),
                   Mod (..)) where


data Inface = N | W
  deriving Show

data Outface = S | E
  deriving Show


data Exp = Unit
         | Tuple Exp Exp
         | Inl   Exp
         | Inr   Exp
         | Iface Inface
  deriving Show


data Command = SendEmpty
             | Send1 Exp Outface
             | Send2 Exp Outface
                     Exp Outface
             | Case  Exp Outface Outface
             | Split Exp
             | Use   String
  deriving Show


type Wire = Maybe Integer

newtype Box = MkBox { unBox :: Inface -> Inface -> Command }

newtype Mod = MkModule { boxes :: [Box] }
