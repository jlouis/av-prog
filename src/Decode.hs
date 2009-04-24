-- Decode Umix instructions

import Data.Bits

opcode_pos :: Int
opcode_pos = 32 - 4

standard_operator_positions :: (Int, Int, Int)
standard_operator_positions = (6, 3, 0)

find_opcode :: Bits a => a -> a
find_opcode w = shiftR w opcode_pos

find_regs :: Bits a => a -> (Int, Int, Int)
find_regs w = (regA, regB, regC)
    where
      regC = w .&. 7
      regB = (shiftR w 3) .&. 7
      regA = (shiftR w 6) .&. 7

data Instruction = Arr_Idx { offset :: Int,
                             ptr :: Int,
                             reg :: Int }
                 | Arr_Update { value :: Int,
                                ptr :: Int,
                                offset :: Int }
                 | Move { reg :: Int,
                          src :: Int,
                          guard ::Int }
                 | Add  { reg :: Int, op1 :: Int, op2 :: Int }
                 | Mul  { reg :: Int, op1 :: Int, op2 :: Int }
                 | Div  { reg :: Int, op1 :: Int, op2 :: Int } -- op1 / op2
                 | Nand { reg :: Int, b :: Int, c :: Int }
                 | Halt
                 | Malloc { size :: Int, reg :: Int }
                 | Free { reg :: Int }
                 | Output { value :: Int }
                 | Input { reg :: Int }
                 | Load { from :: Int, jumppoint :: Int }
                 | LoadImm { value :: Int, reg :: Int }

decode :: Bits a => a -> Maybe Instruction
decode w =
    case find_opcode w of
      -- Standard operators
      0 -> decode_conditional_move w
      1 -> decode_array_index w
      2 -> decode_array_update w
      3 -> decode_add w
      4 -> decode_mul w
      5 -> decode_div w
      6 -> decode_nand w
      -- Other operators
      7 -> decode_halt w
      8 -> decode_malloc w
      9 -> decode_free w
      10 -> decode_output w
      11 -> decode_input w
      12 -> decode_load w
      -- Special operators
      13 -> decode_loadi w
      _ -> Nothing

decode_conditional_move w =
    let (regA, regB, regC) = find_regs w
    in
      Move { reg = regA, guard = regC, value = regB}

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
      Input { value = regC }

decode_load w =
    let (regA, regB, regC) = find_regs w
    in
      Load { from = regB, jumppoint = regC }

decode_loadi :: Bits a => a -> Maybe Instruction
decode_loadi i = Just $ LoadImm { value = value, reg = reg}
    where
      value = shiftR (shiftL i 7) 7
      reg   = shiftR (shiftL i 4) (32 - 7)

