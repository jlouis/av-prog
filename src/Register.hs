module Register (
                 Reg,
                 empty,
                 getReg,
                 writeReg
                )
where

import Data.Word
import Data.Array.IO

{- Register sets as fast arrays -}

type Reg = IOUArray Word32 Word32

empty :: IO Reg
empty = newArray (0, 7) 0

getReg :: Reg -> Word32 -> IO Word32
getReg = readArray

-- Index first, then value
writeReg :: Reg -> Word32 -> Word32 -> IO ()
writeReg = writeArray