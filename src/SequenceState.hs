module SequenceState (State (..)) where

import State

import Data.Word
import Data.Sequence
import qualified Data.Array.IO as A

c_MAX_SIZE = (2 ** 32) - 1


_find f n EmptyL = Nothing
_find f n (x :< xs) =
    if f x then Just (n, x)
    else _find f (n+1) (viewl xs)

find seq f = _find f 0 (viewl seq)

instance State ([Word32], Seq (A.IOUArray Word32 Word32)) where
    empty init = do l <- return $ Prelude.length init
                    program <- A.newListArray (0 :: Word32, fromIntegral (l-1)) init
                    return ([], Data.Sequence.singleton program)

    lookupE (_, s) a off =
        let arr = index s (fromIntegral a) in
        A.readArray arr off

    updateE (n, s) a off v =
        do arr <- return $ index s (fromIntegral a)
           A.writeArray arr off v
           return (n, s)

    allocate (n, s) cap =
        do new_arr <- A.newArray (0, cap) (0 :: Word32)
           case n of
             [] ->
                 do s' <- return $ s |> new_arr
                    return (([], s'), fromIntegral (Data.Sequence.length s' - 1))
             i : n' ->
                 let s' = update (fromIntegral i) new_arr s in
                   return ((n', s'), i)

    copy (n, s) from to =
          do source <- return $ index s (fromIntegral from)
             target <- A.mapArray (\x -> x) source
             return $ (n, update (fromIntegral to) target s)

    free (n, s) x =
        return (x:n, s)

    load s 0 = return $ s
    load s arr = copy s arr 0

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

    copy (n, s) from to =
        let from' = fromIntegral from
        in
          return (n, update (fromIntegral to) (index s from') s)

    free (n, s) x =
        return (x:n, s)

    load s arr = copy s arr 0
