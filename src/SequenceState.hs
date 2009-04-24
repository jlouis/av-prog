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


instance State (Word, Seq (Maybe (Seq Word))) where

    empty initializer = (1, singleton (Just $ fromList initializer))

    lookupE (_, s) a off =
        do s' <- index s (fromIntegral a)
           return $ index s' (fromIntegral off)

    updateE (n, s) a off v =
        let f (Just s') = Just $ update (fromIntegral off) v s'
        in Just (n, adjust f (fromIntegral a) s)

    allocate (n, s) cap =
        let seq = Just $ fromList (Prelude.take (fromIntegral cap) $ repeat 0)
        in
           Just ((n+1, s |> seq), n)

    swap (n, s) a1 a2 =
        let a1' = fromIntegral a1
            a2' = fromIntegral a2
            v1 = index s a1'
            v2 = index s a2'
        in
          Just (n, update a1' v2 (update a2' v1 s))

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

instance State (Word, Seq (Word, (Seq Word))) where

    empty initializer =
        let s = fromList initializer
        in
          (1, singleton (0, s))

    lookupE (nidx, env) arr off =
        do (index', (idx, arr')) <- find env (\(x,_) -> x == arr)
           if (fromIntegral off) >= Data.Sequence.length arr' then 
               error ("lookupE: Index out of bounds: "++
                      (show arr)++","++(show off)++" - length "++
                      (show (Data.Sequence.length arr'))) else 
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

    swap (nidx, env) arr0 arr1 =
        do (idx0, arr0') <- find env (\(x,_) -> x == arr0) ;
           (idx1, arr1') <- find env (\(x,_) -> x == arr1) ;
           return (nidx, update idx1 arr0' (update idx0 arr1' env))

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
