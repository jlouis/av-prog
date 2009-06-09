module Simple.Main (main) where

import SimpleCC
import Simple.Parse
import Simple.Ast
import Twodee.Ast

--printTree ast env = astPrint (envPrint "" env) ast

main = do
  (ast, env, fkt) <- return $ parsePrg "[hello = (s (s z)) + (s z)] Lookup hello * (s (s z))"
  putStrLn $ astPrint "" (start ast env fkt)
--  bxs <- return $ SimpleCC.compile ast
--  nodule <- return $ Twodee.Ast.MkModule { boxes = bxs, modName = "Main" }
--  putStrLn $ show nodule