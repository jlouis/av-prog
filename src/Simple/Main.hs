module Simple.Main (main) where

import SimpleCC
import Simple.Parse
import Simple.Ast
import Twodee.AstRender

--printTree ast env = astPrint (envPrint "" env) st

testProg =
    "Main => [] (Call Fact (s (s (s z)))), "++
    "Fact => [] zeroCheck(Lookup input){(s z)} else "++
    "{(Lookup input) * (Call Fact (Lookup k))}"


main = do
  putStrLn "Parsing.."
  (ast, env, fkt) <- return $ parsePrg testProg
  putStrLn $ "Env: " ++ show env
  putStrLn $ "FuncEnv: " ++ show fkt
  putStrLn $ astPrint "" ast
  putStrLn "Compiling.."
  compiled <- return $ compile ast env fkt
  putStr (show compiled)
  putStrLn "Rendering.."
  rendered <- return $ render compiled "" -- stdlib
  putStrLn rendered


-- Fact
--(Lookup input) * contz(Lookup input){(s z)} else {Call Fact (Pre (Lookup input))}"
--"Main => [hello = (s (s z)) + (s z)] (Lookup hello) * (Call Test (s z)), Test => [] contz(z){(s z)} else {z}"
