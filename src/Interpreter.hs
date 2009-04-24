module Interpreter (interpOps,
                    interpret
                   )
where

import State
import SimpleState
import Decode
import Data.Bits
import Data.Word
import Char

type WordState = (Word, [(Word, Word, Word)])

initStore opcodes =
    let l = length opcodes
        copy store idx [] = store
        copy store idx (opcode : rest) =
            case update store 0 idx opcode of
              Nothing -> error "We have full control of indexes"
              Just newstore -> copy newstore (idx+1) rest

    in case allocate empty (fromIntegral l) of
         Nothing -> error "Can't happen (initStore)"
         Just (ns, 0) -> copy ns 0 opcodes
         Just (ns, k) -> error $ "Can't happen as well (initStore): " ++ (show k)


initRegs :: State s => s
initRegs = empty

interpret' :: State s => [Word] -> IO (s, s)
interpret' opcodes = do
  initial_store <- return $ initStore opcodes
  initial_regs <- return initRegs
  return (initial_store, initial_regs)

interpret :: [Word] -> IO ()
interpret opcodes = do (s, rs) <- (interpret' opcodes :: IO (WordState, WordState))
                       interpOps s rs 0

c_BIT_MASK = 0xFFFFFFFF

interpOps :: State s => s -> s -> Word -> IO ()
interpOps s rs op_ptr =
  do opcode <- case index s 0 op_ptr of
                 Just opc -> return opc
                 Nothing -> error "Could not lookup opcode"
     s <- interpOp s rs op_ptr opcode -- Don't use fromIntegral here
     case s of
       Just (s, rs, op_ptr) -> interpOps s rs op_ptr
       Nothing -> return ()

lookupR rs idx = index rs 0 idx
updateR rs idx val = update rs 0 idx val

interpOpBin s rs op_ptr op1 op2 reg f = do
  op1'    <- lookupR rs op1
  op2'    <- lookupR rs op2
  rs'     <- updateR rs reg (f op1' op2')
  return (s, rs', op_ptr+1)

interpOp :: State s => s -> s -> Word -> Word -> IO (Maybe (s, s, Word))
interpOp s rs op_ptr opc =
    let binop = interpOpBin s rs op_ptr in
    do instr <- case decode opc of
                  Just instr -> return instr
                  Nothing -> error "Opcode decode failure"
       case instr of
         Arr_Idx { ptr=ptr, offset=offset, reg=reg } ->
             return $ do ptr'            <- lookupR rs ptr
                         offset'         <- lookupR rs offset
                         val'            <- index   s  ptr' offset'
                         rs'             <- updateR rs reg val'
                         return (s, rs', op_ptr+1)
         Output { value=value } ->
             case lookupR rs value of
               Just v -> do putStr [chr (fromIntegral v)]
                            return (Just (s, rs, op_ptr+1))
               Nothing -> error "Lookup failure"
         Input { reg=reg } ->
             do c <- getChar
                return $ do rs' <- updateR rs reg $ (fromIntegral . ord) c
                            return (s, rs', op_ptr+1)
         Arr_Update { ptr=ptr, offset=offset, value=value } ->
             return $ do ptr'            <- lookupR   rs ptr
                         offset'         <- lookupR   rs offset
                         val'            <- lookupR   rs value
                         s'              <- update    s  offset' ptr' val'
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
         Malloc { reg=reg, size=size } ->
             return $ do (s', idx) <- allocate s size
                         rs'       <- updateR rs reg idx
                         return (s', rs', op_ptr+1)
         Free { reg=reg } ->
             return $ do idx    <- lookupR rs reg
                         s'     <- free s reg
                         return (s', rs, op_ptr+1)
         Load { from=from, jumppoint=jumppoint } ->
             return $ do from'   <- lookupR rs from
                         s'      <- load s from'
                         return (s', rs, jumppoint)
         LoadImm { value=value, reg=reg } ->
             return $ do rs' <- updateR rs reg value
                         return (s, rs', op_ptr+1)

