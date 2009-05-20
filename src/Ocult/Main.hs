module Ocult.Main (main) where

import Ocult.OcultParser
import Ocult.Ast

import List
import Text.PrettyPrint

main ::   IO()
main = putStr $ render $ docProgram $ program "Add y H => a (A (S (Z S)))"

-- run rule "Add y H => a (A (S (Z S)))"
  

