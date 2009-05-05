module Twodee.Main (main)
where

import Twodee.Layout

main :: IO ()
main = do
  (graph, lookupN, lookupV) <- return $ graphFromProgram testProg
  putStrLn $ show graph
