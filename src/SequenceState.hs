module SequenceState (State (..)) where

import State

import Data.Word
import Data.Sequence
import qualified Data.Array.IO as A

c_MAX_SIZE = (2 ** 32) - 1

instance State ([Word32], A.IOUArray Word32 Word32, Seq (A.IOUArray Word32 Word32)) where
    empty init = do l <- return $! Prelude.length init
                    program <- A.newListArray (0 :: Word32, fromIntegral (l-1)) init
                    return ([], program, Data.Sequence.empty)

    lookupE (_, prg, _) 0 off = A.readArray prg off
    lookupE (_, prg, s) a off =
        let arr = index s (fromIntegral (a-1)) in
        A.readArray arr off

    updateE (n, prg, s) 0 off v = do A.writeArray prg off v
                                     return $! (n, prg, s)
    updateE (n, prg, s) a off v =
        do arr <- return $ index s (fromIntegral (a-1))
           A.writeArray arr off v
           return $! (n, prg, s)

    allocate (n, prg, s) cap =
        do new_arr <- A.newArray (0, cap) (0 :: Word32)
           case n of
             [] ->
                 do s' <- return $! s |> new_arr
                    return (([], prg, s'), fromIntegral (Data.Sequence.length s'))
             i : n' ->
                 let s' = update (fromIntegral (i-1)) new_arr s in
                   return $! ((n', prg, s'), i)

    free (n, prg, s) 0 = error "Can't free the program"
    free (n, prg, s) x =
        do new_arr <-  A.newArray (0, 0) (0 :: Word32)
           let s' = update (fromIntegral x) new_arr s in
             return (x:n, prg, s)
        
 
    load s 0 = return $! s
    load (n, prg, s) arr =
        do source <- return $! index s (fromIntegral (arr-1))
           target <- A.mapArray id source
           return $! (n, target, s)

instance State ([Word32], Seq (Seq Word32)) where

    empty initializer = return ([], singleton (fromList initializer))

    lookupE (_, s) a off =
        let s' = index s (fromIntegral a) in
        return $ index s' (fromIntegral off)

    updateE (n, s) a off v =
        let f (s') = update (fromIntegral off) v s'
        in return (n, adjust f (fromIntegral a) s)

    allocate (n, s) cap =
        let seq  = fromList (Prelude.take (fromIntegral cap) $ repeat 0) in
        case n of 
          [] ->
              let s' = s |> seq in
              return (([], s'), fromIntegral (Data.Sequence.length s' - 1))
          i : n' ->
              let s' = update (fromIntegral i) seq s in
              return ((n', s'), i)


    free (n, s) x =
        return (x:n, s)

    load s arr = copy s arr 0
        where
          copy (n, s) from to =
              let from' = fromIntegral from
              in
                return (n, update (fromIntegral to) (index s from') s)
