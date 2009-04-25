module UmixVM (
               chop_opcodes
              )
where

import Data.Bits
import Data.Word
import Data.Char (ord)

chop_opcodes :: [Char] -> [Word32]
chop_opcodes [] = []
chop_opcodes (c1 : c2 : c3 : c4 : rest) = (fromIntegral $ decode c1 c2 c3 c4) : (chop_opcodes rest)
    where
      decode c1 c2 c3 c4 = (ord c4) + (shiftL (ord c3) 8) + (shiftL (ord c2) 16) + (shiftL (ord c1) 24)
chop_opcodes _ = error "UMix opcodes must be 0 modulo 4"


