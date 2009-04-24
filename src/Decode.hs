-- Decode Umix instructions



module Decode (decode,
               Instruction(..))

where

import Data.Bits

opcode_pos :: Int
opcode_pos = 32 - 4

standard_operator_positions :: (Int, Int, Int)
standard_operator_positions = (6, 3, 0)

find_opcode :: Bits a => a -> a
find_opcode w = shiftR w opcode_pos

find_regs :: Bits a => a -> (a, a, a)
find_regs w = (regA, regB, regC)
    where
      regC = w .&. 7
      regB = (shiftR w 3) .&. 7
      regA = (shiftR w 6) .&. 7

data Instruction a = Arr_Idx { offset :: a,
                             ptr :: a,
                             reg :: a }
                 | Arr_Update { value :: a,
                                ptr :: a,
                                offset :: a }
                 | Move { reg :: a,
                          src :: a,
                          guard ::a }
                 | Add  { reg :: a, op1 :: a, op2 :: a }
                 | Mul  { reg :: a, op1 :: a, op2 :: a }
                 | Div  { reg :: a, op1 :: a, op2 :: a } -- op1 / op2
                 | Nand { reg :: a, b :: a, c :: a }
                 | Halt
                 | Malloc { size :: a, reg :: a }
                 | Free { reg :: a }
                 | Output { value :: a }
                 | Input { reg :: a }
                 | Load { from :: a, jumppoint :: a }
                 | LoadImm { value :: a, reg :: a }

decode :: Bits a => a -> Maybe (Instruction a)
decode w =
    case find_opcode w of
      -- Standard operators
      0 -> return $ decode_conditional_move w
      1 -> return $ decode_array_index w
      2 -> return $ decode_array_update w
      3 -> return $ decode_add w
      4 -> return $ decode_mul w
      5 -> return $ decode_div w
      6 -> return $ decode_nand w
      -- Other operators
      7 -> return $ decode_halt w
      8 -> return $ decode_malloc w
      9 -> return $ decode_free w
      10 -> return $ decode_output w
      11 -> return $ decode_input w
      12 -> return $ decode_load w
      -- Special operators
      13 -> return $ decode_loadi w
      _ -> Nothing

decode_conditional_move :: Bits a => a -> (Instruction a)
decode_conditional_move w =
    let (regA, regB, regC) = find_regs w
    in
      Move { reg = regA, guard = regC, src = regB}

decode_array_index w =
    let (regA, regB, regC) = find_regs w
    in
      Arr_Idx { offset = regC, ptr = regB, reg = regA }

decode_array_update w =
    let (regA, regB, regC) = find_regs w
    in
      Arr_Update { value = regC, offset = regB, ptr = regA }

decode_add w =
    let (regA, regB, regC) = find_regs w
    in
      Add { reg = regA, op1 = regB, op2 = regC }

decode_mul w =
    let (regA, regB, regC) = find_regs w
    in
      Mul{ reg = regA, op1 = regB, op2 = regC }

decode_div w =
    let (regA, regB, regC) = find_regs w
    in
      Div { reg = regA, op1 = regB, op2 = regC }

decode_nand w =
    let (regA, regB, regC) = find_regs w
    in
      Nand { reg = regA, b = regB, c = regC }

decode_halt w = Halt

decode_malloc w =
    let (regA, regB, regC) = find_regs w
    in
      Malloc { size = regC, reg = regB }

decode_free w =
    let (regA, regB, regC) = find_regs w
    in
      Free { reg = regC }

decode_output w =
    let (regA, regB, regC) = find_regs w
    in
      Output { value = regC }

decode_input w =
    let (regA, regB, regC) = find_regs w
    in
      Input { reg = regC }

decode_load w =
    let (regA, regB, regC) = find_regs w
    in
      Load { from = regB, jumppoint = regC }

decode_loadi w = LoadImm { value = value, reg = reg}
    where
      value = shiftR (shiftL w 7) 7
      reg   = shiftR (shiftL w 4) (32 - 7)

