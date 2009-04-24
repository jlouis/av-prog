module SimpleState (State (..)) where 

import State
import List

import Data.Word


c_MAX_SIZE = 2**32-1


instance State (Word, [(Word, Word, Word)]) where

    empty = (1, [])

    index s arr off = 
        let (nextidx, env) = s in 
        do {
          (_,_,val) <- (List.find (\(a,b,c) -> a == arr && b == off ) env) ; 
                       return val 
        } 
        

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