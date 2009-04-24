module SequenceState (State (..)) where 

import State

import Data.Word
import Data.Sequence


c_MAX_SIZE = 2**32-1


_find f n EmptyL = Nothing
_find f n (x :< xs) =
    if f x then Just (n, x)
    else _find f (n+1) (viewl xs)

find seq f = _find f 0 (viewl seq)


instance State (Word, Seq (Word, (Seq Word))) where

    empty initializer =
        let s = fromList initializer
        in
          (1, singleton (0, s))

    lookupE s arr off =
        let (nidx, env) = s in 
        do {
          (index', (idx, arr')) <- find env (\x -> fst x == arr) ; 
          return (index arr' (fromIntegral off))
        }

    updateE s arr off val = 
        let (nidx, env) = s in 
        case find env (\x -> fst x == arr) of 
          Nothing -> Nothing
          Just (index', (idx, arr')) -> 
              let arr'' = update (fromIntegral off) val arr'  in 
              let env'  = update index' (idx, arr'') env in 
              Just (nidx, env')

    allocate s cap = 
        let (nidx, env) = s in 
        let seq = fromList [ 0 | x <- [0..cap] ] in
        let env' = (nidx, seq) <| env in
        Just ((nidx+1, env'), nidx)

    swap s arr0 arr1 = 
        let (nidx, env) = s in 
        do {
          (idx0, arr0') <- find env (\x -> fst x == arr0) ; 
          (idx1, arr1') <- find env (\x -> fst x == arr1) ; 
          return (nidx, update idx1 arr0' (update idx0 arr1' env))
        }

    copy s from to = 
        let (nidx, env) = s in 
        do {
          (idx0, arr0) <- find env (\x -> fst x == from) ; 
          return (nidx, update (fromIntegral to) arr0 env) 
        }

    free s arr = 
        let (nidx, env) = s in 
        case find env (\(idx,_) -> idx == arr) of
          Nothing -> Nothing
          Just (index, (idx, arr)) -> 
              let (head, tail)  = Data.Sequence.splitAt index env in
              let (head' :> _)  = viewr head                      in 
              Just (nidx, head' >< tail)

    load s arr = copy s arr 0 
