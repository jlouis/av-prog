module SequenceState (State (..)) where

import State

import Data.Word
import Data.Sequence


c_MAX_SIZE = (2 ** 32) - 1


_find f n EmptyL = Nothing
_find f n (x :< xs) =
    if f x then Just (n, x)
    else _find f (n+1) (viewl xs)

find seq f = _find f 0 (viewl seq)

instance State ([Word32], Seq (Seq Word32)) where

    empty initializer = ([], singleton (fromList initializer))

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
