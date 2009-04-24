module Interpreter (interpOps,
                    interpret
                   )
where

import State
import SimpleState
import Decode

initStore :: [Int] -> (Int, [(Int, Int, Int)])
initStore opcodes =
    let l = length opcodes
        copy store idx [] = store
        copy store idx (opcode : rest) =
            case update store 0 idx opcode of
              Nothing -> error "We have full control of indexes"
              Just newstore -> copy newstore (idx+1) rest

    in case allocate empty l of
         Nothing -> error "Can't happen (initStore)"
         Just (ns, 0) -> copy ns 0 opcodes
         Just (ns, k) -> error $ "Can't happen as well (initStore): " ++ (show k)


initRegs :: (Int, [(Int, Int, Int)])
initRegs = empty

interpret :: [Int] -> IO ()
interpret opcodes = do
  initial_store <- return $ initStore opcodes
  initial_regs <- return initRegs
  putStrLn "I WILL fail soundly!"
  interpOps initial_store initial_regs 0

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