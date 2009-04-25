module Main (main)
where

import qualified UmixVM as UVM
import qualified Interpreter
import qualified Decode
import Numeric ( showHex )
import System ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  prog <- return $ head args
  contents <- readFile prog
  opcodes <- return $ UVM.chop_opcodes contents
  putStrLn $ show $ Decode.decode $ head opcodes
  Interpreter.interpret opcodes

