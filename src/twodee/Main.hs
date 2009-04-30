module Main (main)
where

import Layout

main :: IO ()
main = do
  (graph, lookupN, lookupV) <- return $ graphFromProgram testProg
  putStr $ show graph
