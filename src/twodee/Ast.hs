-- Abstract Syntax tree
module Ast (Inface (..), Outface (..), Exp (..), Command (..), Wire, Box (..)) where


data Inface = N 
            | W
  deriving Show

data Outface = S
             | E
  deriving Show


data Exp = Empty
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


data Box = MKBox { cmd :: Command, north :: Wire, west :: Wire, east :: Wire, south :: Wire }
  deriving Show

