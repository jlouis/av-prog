module Simple.Main (main) where

import SimpleCC
import Simple.Parse
import Simple.Ast
import Twodee.Ast

main = do
  ast <- return $ parsePrg "(s z) * (s (s z))"
  putStrLn $ astPrint "" (eval ast)
  bxs <- return $ SimpleCC.compile ast
  nodule <- return $ Twodee.Ast.MkModule { boxes = bxs, modName = "Main" }
  putStrLn $ show nodule

