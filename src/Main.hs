module Main (main)
where

import qualified UmixVM as UVM

main :: IO ()
main = do
  f <- return UVM.foo
  putStrLn f
