module Ocult.Main (main) where

import Ocult.OcultParser
import Ocult.Ast
import List

main ::   IO()
main = head $ program "Add y H => a (A (S (Z S)))"

-- run rule "Add y H => a (A (S (Z S)))"
  

