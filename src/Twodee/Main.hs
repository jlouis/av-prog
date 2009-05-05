module Twodee.Main (main)
where

import Twodee.Layout

main :: IO ()
main = do
  putStr (show (layoutProgram testProg))

