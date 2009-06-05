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
         | Tuple { e_exp0 :: Exp,  e_exp1 :: Exp }
         | Inl   { e_exp  :: Exp }
         | Inr   { e_exp  :: Exp }
         | Iface Inface
  deriving Show


data Command = SendEmpty
             | Send1 { c_exp  :: Exp, out  :: Outface }
             | Send2 { c_exp0 :: Exp, out1 :: Outface,
                       c_exp1 :: Exp, out2 :: Outface }
             | Case  { c_exp  :: Exp, out1 :: Outface, out2 :: Outface }
             | Split { c_exp  :: Exp }
             | Use   { name   :: String }
  deriving Show


type Wire = Maybe Integer


data Box = MkBox { cmd :: Command, north :: Wire, west :: Wire, east :: Wire, south :: Wire }
  deriving Show

newtype Mod = MkModule { boxes :: [Box] }
