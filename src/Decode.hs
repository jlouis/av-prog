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

decode :: (Bits a, Ord a) => a -> Maybe (Instruction a)
decode w =
   let (regA, regB, regC) = find_regs w
   in case find_opcode w of
        k | k >= 0 && k < 13 ->
              return $ case k of
                         -- Standard operators
                         0 -> Move { reg = regA, guard = regC, src = regB}
                         1 -> Arr_Idx { offset = regC, ptr = regB, reg = regA }
                         2 -> Arr_Update { value = regC, offset = regB, ptr = regA }
                         3 -> Add { reg = regA, op1 = regB, op2 = regC }
                         4 -> Mul{ reg = regA, op1 = regB, op2 = regC }
                         5 -> Div { reg = regA, op1 = regB, op2 = regC }
                         6 -> Nand { reg = regA, b = regB, c = regC }
                         -- Other operators
                         7 -> Halt
                         8 -> Malloc { size = regC, reg = regB }
                         9 -> Free { reg = regC }
                         10 -> Output { value = regC }
                         11 -> Input { reg = regC }
                         12 -> Load { from = regB, jumppoint = regC }
        k | k == 13 -> return $ decode_loadi w
        k | otherwise -> Nothing

decode_loadi :: Bits a => a -> (Instruction a)
decode_loadi w = LoadImm { value = value, reg = reg}
    where
      value = shiftR (shiftL w 7) 7
      reg   = shiftR (shiftL w 4) (32 - 7)
