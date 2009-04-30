module Main (main) 
where

import Layout


main :: IO ()
main = do
  let (graph, lookupN, lookupV) = (graphFromProgram testProg) in 
    putStr (show graph)