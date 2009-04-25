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
        in Just (n, adjust f (fromIntegral a) s)

    allocate (n, s) cap =
        let seq  = fromList (Prelude.take (fromIntegral cap) $ repeat 0) in
        case n of 
          [] ->
              let s' = s |> seq in 
              Just (([], s'), fromIntegral (Data.Sequence.length s' - 1))
          i : n' -> 
              let s' = update (fromIntegral i) seq s in
              Just ((n', s'), i)

    copy (n, s) from to =
        let from' = fromIntegral from
        in
          Just (n, update (fromIntegral to) (index s from') s)

    free (n, s) x =
        Just (x:n, s)

    load s arr = copy s arr 0






instance State (Word32, Seq (Maybe (Seq Word32))) where

    empty initializer = (1, singleton (Just $! fromList initializer))

    lookupE (_, s) a off =
        do s' <- index s (fromIntegral a)
           return $! index s' (fromIntegral off)

    updateE (n, s) a off v =
        let f (Just s') = Just $! update (fromIntegral off) v s'
        in Just (n, adjust f (fromIntegral a) s)

    allocate (n, s) cap =
        let seq = Just $! fromList (Prelude.take (fromIntegral cap) $ repeat 0)
            news = s |> seq
        in
          if Data.Sequence.length news == (fromIntegral (n+1)) then Just ((n+1, s |> seq), n) else error "FAIL"

    copy (n, s) from to =
        let from' = fromIntegral from
        in
          Just (n, update (fromIntegral to) (index s from') s)

    free (n, s) x =
        let f Nothing = error "Double free"
            f (Just e) = Nothing
        in
          Just (n, adjust f (fromIntegral x) s)

    load s arr = copy s arr 0

instance State (Word32, Seq (Word32, (Seq Word32))) where

    empty initializer =
        let s = fromList initializer
        in
          (1, singleton (0, s))

    lookupE (nidx, env) arr off =
        do (index', (idx, arr')) <- find env (\(x,_) -> x == arr)
           if (fromIntegral off) >= Data.Sequence.length arr' then 
               error ("lookupE: Index out of bounds: "++
                      (show arr)++","++(show off)++" - length "++
                      (show (Data.Sequence.length arr'))++ " - env size "++
                      (show (Data.Sequence.length env))) else 
               return (index arr' (fromIntegral off))

    updateE (nidx, env) arr off val =
        do (index', (idx, arr')) <- find env (\x -> fst x == arr)
           arr'' <- return $ update (fromIntegral off) val arr'
           env' <- return $ update index' (idx, arr'') env
           return $ (nidx, env')

    allocate (nidx, env) cap =
        let seq = fromList [ 0 | x <- [0..cap] ]
            env' = (nidx, seq) <| env
        in
          Just ((nidx+1, env'), nidx)

    copy (nidx, env) from to =
        do (_, arr0) <- find env (\(x,_) -> x == from) ;
           (idx1, _) <- find env (\(x,_) -> x == to)   ;
           return (nidx, update idx1 arr0 env)

    free (nidx, env) arr =
        do (index, (idx, arr)) <- find env (\(idx, _) -> idx == arr)
           (head, tail) <- return $ Data.Sequence.splitAt (index+1) env
           case viewr head of
             EmptyR     -> return (nidx, tail)
             head' :> _ -> return $ (nidx, head' >< tail)

    load s arr = copy s arr 0
