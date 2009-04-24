module Interpreter (interpOps) where

import SimpleState
import Decode

import Data.Bits
import Data.Word
import Char
import Control.Applicative


c_BIT_MASK = 0xFFFFFFFF


interpOps s rs op_ptr =
    case index s 0 op_ptr of
      Just opc ->
          case interpOp s rs op_ptr opc of
            Just (s, rs, op_ptr) -> interpOps s rs op_ptr
            Nothing -> error "Interpretation failed"
      Nothing -> error "Could not lookup opcode"

lookupR rs idx = do
      val <- index rs 0 idx ;
      return val

updateR rs idx val =
    update rs 0 idx val



interpOpBin s rs op_ptr op1 op2 reg f = do
  op1'    <- lookupR rs op1
  op2'    <- lookupR rs op2
  rs'     <- updateR rs reg (f op1' op2')
  return (s, rs', op_ptr+1)

interpOp s rs op_ptr opc =
    let binop = interpOpBin s rs op_ptr in
    case decode opc of
      Just instr ->
          case instr of

            Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
                do {
                  ptr'            <- lookupR rs ptr            ;
                  offset'         <- lookupR rs offset         ;
                  val'            <- index   s  ptr' offset'   ;
                  rs'             <- updateR rs reg val'     ;
                  return (s, rs', op_ptr+1)
                }

            Arr_Update { ptr=ptr, offset=offset, value=value } ->
                do {
                  ptr'            <- lookupR   rs ptr               ;
                  offset'         <- lookupR   rs offset            ;
                  val'            <- lookupR   rs value           ;
                  s'              <- update    s  offset' ptr' val' ;
                  return (s', rs, op_ptr+1)
                }

            Move { src=src, reg=reg, guard=guard } ->
                do {
                  guard'          <- lookupR   rs guard ;
                  case guard' of
                    0 -> return (s, rs, op_ptr+1)
                    _ -> do {
                           src' <- lookupR rs src      ;
                           rs'  <- updateR rs reg src' ;
                           return (s, rs', op_ptr+1)
                         }
                }

            Add { reg=reg, op1=op1, op2=op2 } ->
                binop op1 op2 reg (\x y -> (x + y) .&. c_BIT_MASK)

            Mul { reg=reg, op1=op1, op2=op2 } ->
                binop op1 op2 reg (\x y -> (x * y) .&. c_BIT_MASK)

            Div { reg=reg, op1=op1, op2=op2 } ->
                binop op1 op2 reg (\x y -> (div x y))

            Nand { reg=reg, b=op1, c=op2 } ->
                binop op1 op2 reg (\x y -> (complement (x .&. y) :: Word))

            Halt ->
                Nothing

            Malloc { reg=reg, size=size } ->
                do {
                  (s', idx) <- allocate s size ;
                  rs'       <- updateR rs reg idx ;
                  return (s', rs', op_ptr+1)
                }

            Free { reg=reg } ->
                do {
                  idx    <- lookupR rs reg ;
                  s'     <- free s reg  ;
                  return (s', rs, op_ptr+1)
                }

            Output { value=value } ->
                do {
                  value' <- lookupR rs value ;
                  let _ = putStr [chr (fromEnum value')] in
                  return (s, rs, op_ptr+1)
                }

            {-
            Input { reg=reg } ->
                do {
                  char <- getChar ;
                }
               -}

            Load { from=from, jumppoint=jumppoint } ->
                do {
                  from'   <- lookupR rs from ;
                  s'      <- load s from'    ;
                  return (s', rs, jumppoint)
                }

            LoadImm { value=value, reg=reg } ->
                do {
                  rs' <- updateR rs reg value ;
                  return (s, rs', op_ptr+1)
                }




            _ -> error "Unknown opcode"
      Nothing -> error "Opcode out of range"