module Interpreter (interpOps) where

import SimpleState
import Decode


interpOps s rs op_ptr = 
    case index s 0 op_ptr of
      Just opc ->
          case interpOp s rs op_ptr opc of
            Just (s, rs, op_ptr) -> interpOps s rs op_ptr
            Nothing -> error "Interpretation failed"
      Nothing -> error "Could not lookup opcode"


interpOp s rs op_ptr opc = 
    case decode opc of 
      Just instr -> 
          case instr of 
            Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
                do {
                  offset' <- index  rs 0    offset    ;
                  ptr'    <- index  rs 0    ptr       ;
                  val'    <- index  s  ptr' offset'   ;
                  rs'     <- update rs 0 reg val'     ;
                  return (s, rs', op_ptr+1)
                }
            _ -> error "Unknown opcode"
      Nothing -> error "Opcode out of range"