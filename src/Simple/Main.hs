module Simple.Main (main) where

import SimpleCC
import Simple.Parse
import Simple.Ast
import Twodee.Ast

--printTree ast env = astPrint (envPrint "" env) st

main = do
  (ast, env, fkt) <- return $ parsePrg "Main => [] (Call Fact (s(s (s z)))), Fact => [] zeroCheck(Lookup input){(s z)} else {(Lookup input) * (Call Fact (Lookup k))}"
-- Fact
--(Lookup input) * contz(Lookup input){(s z)} else {Call Fact (Pre (Lookup input))}"

--"Main => [hello = (s (s z)) + (s z)] (Lookup hello) * (Call Test (s z)), Test => [] contz(z){(s z)} else {z}"
  putStrLn $ astPrint "" (start ast env fkt)
--  bxs <- return $ SimpleCC.compile astg
--  nodule <- return $ Twodee.Ast.MkModule { boxes = bxs, modName = "Main" }
--  putStrLn $ show nodule