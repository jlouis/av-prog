module SequenceState (State (..)) where 

import State

import Data.Word
import Data.Sequence


c_MAX_SIZE = 2**32-1


_find f n EmptyL = Nothing
_find f n (x :< xs) =
    if f x then Just n
    else _find f (n+1) (viewl xs)

find seq f = _find f 0 (viewl seq)


instance State (Word, Seq (Word, (Seq Word))) where

    empty = (1, fromList [])
{-
    lookupE s arr off =
        let (next, env) = s in 
        case find env (\x -> fst x == arr) of
          Nothing -> Nothing
          Just index' -> 
              let (idx, arr') = index env (fromIntegral index') in
              Just (index arr' (fromIntegral off))

    update s arr off val = 
        let (next, env) = s in 
        case find env (\x -> fst x == arr) of 
          Nothing -> Nothing
          Just index' -> 
              
                  
        


    update s arr off val = 
        let (nextidx, env) = s in 
        Just (nextidx, ((arr, off, val) : filter (\(a,b,_) -> a /= arr || b /= off) env))


    allocate s cap = 
        let (idx, env) = s in 
        let nextidx = idx + 1 in 
        Just ((nextidx, ([(idx, i, 0) | i <- [0..cap]] ++ env)), idx)


    swap s arr0 arr1 =
         let (nextidx, env) = s in 
         let env' = map (\(arr, off, val) -> 
                            if arr == arr0 then
                                (arr1, off, val)
                            else if arr == arr1 then
                                     (arr0, off, val)
                                 else (arr, off, val)) env
         in Just (nextidx, env')


    copy s from to = 
        let (nextidx, env) = s in 
        let env' = map (\(_,off,val) -> (to, off, val)) 
                  (filter (\(arr,_,_) -> arr == from) env) ++ 
                  (filter (\(arr,_,_) -> arr /= to) env) 
        in Just (nextidx, env')


    free s arr = 
        let (nextidx, env) = s in 
        Just (nextidx, filter (\(a,b,c) -> a /= arr) env)

    
    load s arr = copy s arr 0 
    -}