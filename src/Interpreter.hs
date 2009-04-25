module Interpreter (interpOps,
                    interpret
                   )
where

import State
import SimpleState
import SequenceState
import Decode
import Data.Bits
import Data.Word
import Data.Sequence (Seq)
import Char
import Numeric (showHex)

import qualified Register as R

-- Change this to use the new Sequence State system
type WordState = ([Word32], Seq (Maybe (Seq Word32)))
--type WordState = (Word32, Seq (Word32, (Seq Word32))) 

initStore :: State s => [Word32] -> s
initStore opcodes = empty opcodes

initRegs :: State s => s
initRegs = empty [0 | x <- [1..8]]


interpret' :: State s => [Word32] -> IO (s, R.Reg)
interpret' opcodes = do
  initial_store <- return $ initStore opcodes
  initial_regs <- R.empty
  return (initial_store, initial_regs)

interpret :: [Word32] -> IO ()
interpret opcodes = do (s, rs) <- (interpret' opcodes :: IO (WordState, R.Reg))
                       interpOps s rs 0

interpOps :: State s => s -> R.Reg -> Word32 -> IO ()
interpOps s rs op_ptr =
  do opcode <- case lookupE s 0 op_ptr of
                 Just opc -> return opc
                 Nothing -> error "Could not lookup opcode" 
     s <- interpOp s rs op_ptr opcode -- Don't use fromIntegral here
     case s of
       Just (s, op_ptr) -> interpOps s rs op_ptr
       Nothing -> return ()


lookupR :: State s => s -> Word32 -> Maybe Word32
lookupR rs idx = lookupE rs 0 idx

updateR :: State s => s -> Word32 -> Word32 -> Maybe s
updateR rs idx val = updateE rs 0 idx val

interpOpBin :: State s =>
               s -> R.Reg -> Word32 -> Word32 -> Word32 -> Word32
                 -> (Word32 -> Word32 -> Word32)
                 -> IO (Maybe (s, Word32))
interpOpBin s rs op_ptr op1 op2 reg f = do
  op1'    <- R.getReg rs op1
  op2'    <- R.getReg rs op2
  R.writeReg rs reg (f op1' op2')
  return $ Just (s, op_ptr+1)


pad orig = (take (8-(length orig)) (repeat '0')) ++ orig

interpOp :: State s => s -> R.Reg -> Word32 -> Word32 -> IO (Maybe (s, Word32))
interpOp s rs op_ptr opc =
    let binop = interpOpBin s rs op_ptr in
    do instr <- case decode opc of
                  Just instr -> return instr
                  Nothing -> error "Opcode decode failure"
--       putStrLn $ (show instr)  ++ "\t" ++ (pad ( showHex opc ""))
       case instr of
         Move { src=src, reg=reg, guard=guard } ->
             do guard'          <- R.getReg rs guard
                case guard' of
                  0 -> return $ Just (s, op_ptr+1)
                  _ -> do v <- R.getReg rs src
                          R.writeReg rs reg v
                          return $ Just (s, op_ptr+1)
         Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
             do ptr'            <- R.getReg rs ptr
                offset'         <- R.getReg rs offset
                val'            <- case lookupE s  ptr' offset' of
                                     Just v -> return v
                                     Nothing -> error "Array out of bounds"
                R.writeReg rs reg val'
                return $ Just (s, op_ptr+1)
         Arr_Update { ptr=ptr, offset=offset, value=value } ->
             do ptr' <- R.getReg rs ptr
                offset' <- R.getReg rs offset
                val' <- R.getReg rs value
                s' <- case updateE s ptr' offset' val' of
                        Just s' -> return s'
                        Nothing -> error "Arr_Update error"
                return $ Just (s', op_ptr+1)
         Add { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg (+)
         Mul { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg (*)
         Div { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg div
         Nand { reg=reg, b=op1, c=op2 } -> binop op1 op2 reg (\x y -> (complement (x .&. y)))
         Halt -> return Nothing
         Malloc { reg=reg, size=size } ->
             do sz <- R.getReg rs size
                (s', idx) <- case allocate s sz of
                               Just (s', idx) -> return (s', idx)
                               Nothing -> error "Allocation failure"
                R.writeReg rs reg idx
                return $ Just (s', op_ptr+1)
         Free { reg=reg } ->
             do idx <- R.getReg rs reg
                s' <- case free s idx of
                        Just s' -> return s'
                        Nothing -> error "Abandonment failure"
                return $ Just (s', op_ptr+1)
         Output { value=value } ->
             do v <- R.getReg rs value
                putChar (chr (fromIntegral v))
                return $ Just (s, op_ptr+1)
         Input { reg=reg } ->
             do c <- getChar
                R.writeReg rs reg $ (fromIntegral . ord) c
                return $ Just (s, op_ptr+1)
         Load { from=from, jumppoint=jumppoint } ->
             do f <- R.getReg rs from
                j <- R.getReg rs jumppoint
                s' <- case load s f of
                        Just s' -> return s'
                        Nothing -> error "Couldn't load"
                return $ Just (s', j)
         LoadImm { value=value, reg=reg } ->
             do R.writeReg rs reg value
                return $ Just (s, op_ptr+1)

