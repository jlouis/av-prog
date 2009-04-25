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
<<<<<<< HEAD:src/Interpreter.hs
<<<<<<< HEAD:src/Interpreter.hs
type WordState = ([Word32], Seq (Maybe (Seq Word32)))
--type WordState = (Word32, Seq (Word32, (Seq Word32))) 
=======
type WordState = (Word32, Seq (Maybe (Seq Word32)))
--type WordState = (Word, Seq (Word, (Seq Word))) 
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
=======
type WordState = (Word, Seq (Maybe (Seq Word)))
--type WordState = (Word, Seq (Word, (Seq Word))) 
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs

initStore :: State s => [Word] -> s
initStore opcodes =
    empty opcodes

<<<<<<< HEAD:src/Interpreter.hs
initRegs :: State s => s
initRegs = empty [0 | x <- [1..8]]

<<<<<<< HEAD:src/Interpreter.hs
interpret' :: State s => [Word32] -> IO (s, s)
=======
interpret' :: State s => [Word32] -> IO (s, R.Reg)
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
=======
interpret' :: State s => [Word] -> IO (s, s)
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs
interpret' opcodes = do
  initial_store <- return $ initStore opcodes
  initial_regs <- R.empty
  return (initial_store, initial_regs)

<<<<<<< HEAD:src/Interpreter.hs
interpret :: [Word32] -> IO ()
<<<<<<< HEAD:src/Interpreter.hs
interpret opcodes = do (s, rs) <- (interpret' opcodes :: IO (WordState, WordState))
                       interpOps s rs 0

interpOps :: State s => s -> s -> Word32 -> IO ()
=======
interpret opcodes = do (s, rs) <- (interpret' opcodes :: IO (WordState, R.Reg))
                       interpOps s rs 0

c_BIT_MASK :: Word32
c_BIT_MASK = 0xFFFFFFFF

interpOps :: State s => s -> R.Reg -> Word32 -> IO ()
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
=======
interpret :: [Word] -> IO ()
interpret opcodes = do (s, rs) <- (interpret' opcodes :: IO (WordState, WordState))
                       interpOps s rs 0

c_BIT_MASK :: Word
c_BIT_MASK = 0xFFFFFFFF

interpOps :: State s => s -> s -> Word -> IO ()
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs
interpOps s rs op_ptr =
  do opcode <- case lookupE s 0 op_ptr of
                 Just opc -> return opc
                 Nothing -> error "Could not lookup opcode" 
     s <- interpOp s rs op_ptr opcode -- Don't use fromIntegral here
     case s of
       Just (s, rs, op_ptr) -> interpOps s rs op_ptr
       Nothing -> return ()

<<<<<<< HEAD:src/Interpreter.hs
<<<<<<< HEAD:src/Interpreter.hs
lookupR :: State s => s -> Word32 -> Maybe Word32
=======
lookupR :: State s => s -> Word -> Maybe Word
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs
lookupR rs idx = lookupE rs 0 idx

updateR :: State s => s -> Word -> Word -> Maybe s
updateR rs idx val = updateE rs 0 idx val

interpOpBin :: State s =>
<<<<<<< HEAD:src/Interpreter.hs
               s -> s -> Word32 -> Word32 -> Word32 -> Word32
                 -> (Word32 -> Word32 -> Word32)
                 -> Maybe (s, s, Word32)
=======
interpOpBin :: State s =>
               s -> R.Reg -> Word32 -> Word32 -> Word32 -> Word32
                 -> (Word32 -> Word32 -> Word32)
                 -> IO (Maybe (s, R.Reg, Word32))
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
=======
               s -> s -> Word -> Word -> Word -> Word
                 -> (Word -> Word -> Word)
                 -> Maybe (s, s, Word)
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs
interpOpBin s rs op_ptr op1 op2 reg f = do
  op1'    <- R.getReg rs op1
  op2'    <- R.getReg rs op2
  R.writeReg rs reg (f op1' op2')
  return $ Just (s, rs, op_ptr+1)

<<<<<<< HEAD:src/Interpreter.hs
<<<<<<< HEAD:src/Interpreter.hs
interpOp :: State s => s -> s -> Word32 -> Word32 -> IO (Maybe (s, s, Word32))
=======
interpOp :: State s => s -> R.Reg -> Word32 -> Word32 -> IO (Maybe (s, R.Reg, Word32))
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
=======
interpOp :: State s => s -> s -> Word -> Word -> IO (Maybe (s, s, Word))
>>>>>>> 16522ce... Word32 in Interpreter:src/Interpreter.hs
interpOp s rs op_ptr opc =
    let binop = interpOpBin s rs op_ptr in
    do instr <- case decode opc of
                  Just instr -> return instr
                  Nothing -> error "Opcode decode failure"
       putStrLn $ let i = show instr
                      orig = showHex opc ""
                  in i ++ "\t\t" ++ orig
       case instr of
         Move { src=src, reg=reg, guard=guard } ->
             do guard'          <- R.getReg rs guard
                case guard' of
                  0 -> return $ Just (s, rs, op_ptr+1)
                  _ -> do v <- R.getReg rs src
                          R.writeReg rs reg v
                          return $ Just (s, rs, op_ptr+1)
         Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
             do ptr'            <- R.getReg rs ptr
                offset'         <- R.getReg rs offset
                val'            <- case lookupE s  ptr' offset' of
                                     Just v -> return v
                                     Nothing -> error "Array out of bounds"
                R.writeReg rs reg val'
                return $ Just (s, rs, op_ptr+1)
         Arr_Update { ptr=ptr, offset=offset, value=value } ->
<<<<<<< HEAD:src/Interpreter.hs
             return $ do ptr'            <- lookupR   rs ptr
                         offset'         <- lookupR   rs offset
                         val'            <- lookupR   rs value
                         s'              <- updateE   s  ptr' offset' val'
                         return (s', rs, op_ptr+1)
         Move { src=src, reg=reg, guard=guard } ->
             return $ do guard'          <- lookupR   rs guard
                         case guard' of
                           0 -> return (s, rs, op_ptr+1)
                           _ -> do src' <- lookupR rs src
                                   rs'  <- updateR rs reg src'
                                   return (s, rs', op_ptr+1)
         Add { reg=reg, op1=op1, op2=op2 } ->
             return $ binop op1 op2 reg (\x y -> (x + y) .&. c_BIT_MASK)
         Mul { reg=reg, op1=op1, op2=op2 } ->
             return $ binop op1 op2 reg (\x y -> (x * y) .&. c_BIT_MASK)
         Div { reg=reg, op1=op1, op2=op2 } ->
             return $ binop op1 op2 reg (\x y -> (div x y))
         Nand { reg=reg, b=op1, c=op2 } ->
             return $ binop op1 op2 reg (\x y -> (complement (x .&. y) :: Word))
         Halt ->
             return Nothing
=======
             do ptr' <- R.getReg rs ptr
                offset' <- R.getReg rs offset
                val' <- R.getReg rs value
                s' <- case updateE s ptr' offset' val' of
                        Just s' -> return s'
                        Nothing -> error "Arr_Update error"
                return $ Just (s', rs, op_ptr+1)
         Add { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg (+)
         Mul { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg (*)
         Div { reg=reg, op1=op1, op2=op2 } -> binop op1 op2 reg div
         Nand { reg=reg, b=op1, c=op2 } -> binop op1 op2 reg (\x y -> (complement (x .&. y)))
         Halt -> return Nothing
>>>>>>> c2deaccbfdf818ecb40729d1060c3f824473d649:src/Interpreter.hs
         Malloc { reg=reg, size=size } ->
             do sz <- R.getReg rs size
                (s', idx) <- case allocate s sz of
                               Just (s', idx) -> return (s', idx)
                               Nothing -> error "Allocation failure"
                R.writeReg rs reg idx
                return $ Just (s', rs, op_ptr+1)
         Free { reg=reg } ->
             do idx <- R.getReg rs reg
                s' <- case free s idx of
                        Just s' -> return s'
                        Nothing -> error "Abandonment failure"
                return $ Just (s', rs, op_ptr+1)
         Output { value=value } ->
             do v <- R.getReg rs value
                return $ Just (s, rs, op_ptr+1)
         Input { reg=reg } ->
             do c <- getChar
                R.writeReg rs reg $ (fromIntegral . ord) c
                return $ Just (s, rs, op_ptr+1)
         Load { from=from, jumppoint=jumppoint } ->
             do f <- R.getReg rs from
                j <- R.getReg rs jumppoint
                s' <- case load s f of
                        Just s' -> return s'
                        Nothing -> error "Couldn't load"
                return $ Just (s', rs, j)
         LoadImm { value=value, reg=reg } ->
             do R.writeReg rs reg value
                return $ Just (s, rs, op_ptr+1)

