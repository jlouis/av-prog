-- Abstract Syntax tree
module AST (Inface, Outface, Exp, Command) where

data Inface = N
            | W

data Outface = S
             | E

data Exp   = Empty
           | Tuple {exp1_e :: Exp,  exp2_e :: Exp} 
           | Inl {exp_e :: Exp}
           | Inr {exp_e :: Exp}
           | Inface

data Command = SendEmpty
             | Send1 {exp_c :: Exp,  out :: Outface}
             | Send2 {exp1_c :: Exp,  out1 :: Outface,
                      exp2_c :: Exp,  out2 :: Outface}
             | Case {exp_c :: Exp,  out1 :: Outface, out2 :: Outface}
             | Split {exp_c :: Exp}
             | Use {name :: String}