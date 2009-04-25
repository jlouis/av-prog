module Interpreter (
                    interpret
                   )
where

import State
import SequenceState
import Decode
import Data.Bits
import Data.Word
import Data.Array.IO
import Data.Sequence (Seq)
import Char
import Numeric (showHex)

import qualified Register as R

-- Change this to use the new Sequence State system
type WordState = IO ([Word32], IOUArray Word32 Word32, Seq (IOUArray Word32 Word32))
--type WordState = (Word32, Seq (Word32, (Seq Word32))) 

interpret :: [Word32] -> IO ()
interpret opcodes = do initial_store <- (empty opcodes :: WordState)
                       initial_regs <- R.empty
                       interp initial_store initial_regs 0

interpOpBin :: State s =>
               s -> R.Reg -> Word32 -> Word32 -> Word32 -> Word32
                 -> (Word32 -> Word32 -> Word32)
                 -> IO ()
interpOpBin s rs op_ptr op1 op2 reg f = do
  op1'    <- (R.getReg rs) op1
  op2'    <- (R.getReg rs) op2
  R.writeReg rs reg (f op1' op2')
  interp s rs (op_ptr+1)

pad orig = (take (8-(length orig)) (repeat '0')) ++ orig

interp :: State s => s -> R.Reg -> Word32 -> IO ()
interp s rs (-1)   = error "Wrapped around!"
interp s rs op_ptr =
    let binop = interpOpBin s rs op_ptr
    in
    do opcode <- lookupE s 0 op_ptr
       instr <- return $ decode opcode
--       putStrLn $ (pad ( showHex opcode "")) ++ "\t" ++ (show op_ptr) ++ "\t" ++ (show instr) 
       case instr of
         Move { src=src, reg=reg, guard=guard } ->
             do guard'          <- R.getReg rs guard
                case guard' of
                  0 -> interp s rs (op_ptr+1)
                  _ -> do v <- R.getReg rs src
                          R.writeReg rs reg v
                          interp s rs (op_ptr+1)
         Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
             do ptr'            <- R.getReg rs ptr
                offset'         <- R.getReg rs offset
                val'            <- lookupE s ptr' offset'
                R.writeReg rs reg val'
                interp s rs (op_ptr+1)
         Arr_Update { ptr=ptr, offset=offset, value=value } ->
             do ptr' <- R.getReg rs ptr
                offset' <- R.getReg rs offset
                val' <- R.getReg rs value
                s' <- updateE s ptr' offset' val'
                interp s' rs (op_ptr+1)
         Add { reg=reg, op1=op1, op2=op2 } -> (binop op1 op2 reg) $! (+)
         Mul { reg=reg, op1=op1, op2=op2 } -> (binop op1 op2 reg) $! (*)
         Div { reg=reg, op1=op1, op2=op2 } -> (binop op1 op2 reg) $! div
         Nand { reg=reg, b=op1, c=op2 }    -> (binop op1 op2 reg) $! (\x y -> (complement (x .&. y)))
         Halt -> putStrLn "*** HALT ***"
         Malloc { reg=reg, size=size } ->
             do sz <- R.getReg rs size
                (s', idx) <- allocate s sz
                R.writeReg rs reg idx
                interp s' rs (op_ptr+1)
         Free { reg=reg } ->
             do idx <- R.getReg rs reg
                s' <- free s idx
                interp s' rs (op_ptr+1)
         Output { value=value } ->
             do v <- R.getReg rs value
                putChar (chr (fromIntegral v))
                interp s rs (op_ptr+1)
         Input { reg=reg } ->
             do c <- getChar
                R.writeReg rs reg $ (fromIntegral . ord) c
                interp s rs (op_ptr+1)
         Load { from=from, jumppoint=jumppoint } ->
             do f <- R.getReg rs from
                j <- R.getReg rs jumppoint
                s' <- load s f
                interp s' rs j
         LoadImm { value=value, reg=reg } ->
             do R.writeReg rs reg value
                interp s rs (op_ptr+1)

