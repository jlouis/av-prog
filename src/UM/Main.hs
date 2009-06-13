module UM.Main (main)
where

import qualified UmixVM as UVM
import qualified Interpreter
import System ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  prog <- return $ head args
  contents <- readFile prog
  opcodes <- return $ UVM.chop_opcodes contents
  Interpreter.interpret opcodes

