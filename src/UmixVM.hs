module UmixVM ()
where

import Interpreter

import Data.Word


foo :: [ Word ]
foo =  [ 0x70000000 ]


main :: IO ()
main = do
  putStr "good"
