module Layout where

import Data.Graph
import Ast

testProg = [ 
 MKBox {cmd = Send2 {c_exp0 = (Iface W), out1 = S, 
                     c_exp1 = (Iface W), out2 = E}, 
        n = Nothing, w = Just 0, e = Just 3, s = Just 2}, 

 MKBox {cmd = Case {c_exp = (Iface N), out1 = S, out2 = E},
        n = Just 1, w = Nothing, e = Just 5, s = Just 4}, 

 MKBox {cmd = Use { name = "plus" }, 
        n = Just 4, w = Just 2,  e = Just 6, s = Nothing},

 MKBox {cmd = Send1 {c_exp = Inl {e_exp = (Iface W)}, out = E}, 
        n = Just 3, w = Just 6, e = Just 7, s = Nothing},

 MKBox {cmd = Send1 {c_exp = (Iface N), out = E},
        n = Nothing, w = Just 5, e = Just 8, s = Nothing}

 ]