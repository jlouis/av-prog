module Main (main)
where

import qualified UmixVM as UVM
import System ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  prog <- return $ head args
  contents <- readFile prog
  putStrLn $ show $ length contents
