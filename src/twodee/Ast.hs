-- Abstract Syntax tree
module Ast (Inface (..), Outface (..), Exp (..), Command (..), Wire, Box (..)) where

data Inface = N 
            | W

data Outface = S
             | E

data Exp = Empty
         | Tuple { e_exp0 :: Exp,  e_exp1 :: Exp }
         | Inl   { e_exp  :: Exp }
         | Inr   { e_exp  :: Exp }
         | Iface Inface

data Command = SendEmpty
             | Send1 { c_exp  :: Exp, out  :: Outface }
             | Send2 { c_exp0 :: Exp, out1 :: Outface,
                       c_exp1 :: Exp, out2 :: Outface }
             | Case  { c_exp  :: Exp, out1 :: Outface, out2 :: Outface }
             | Split { c_exp  :: Exp }
             | Use   { name   :: String }


type Wire = Maybe Integer

data Box = MKBox { cmd :: Command, n :: Wire, w :: Wire, e :: Wire, s :: Wire }

data Module 
